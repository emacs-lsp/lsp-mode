param (
    [string]$ServerName = ".",
    [Parameter(Mandatory=$true)][string]$PipeName
)

# Use named pipe as stdin/out

$Source = @"
using System;
using System.IO;
using System.IO.Pipes;
using System.Threading.Tasks;

public static class StdPipe
{
    public static void RouteToPipe(string pipeServer, string pipeName)
    {
        var pipeClient = new NamedPipeClientStream(pipeServer, pipeName, PipeDirection.InOut, PipeOptions.Asynchronous);
        pipeClient.Connect();

        var pipeReader = new StreamReader(pipeClient, new System.Text.UTF8Encoding(false));
        var pipeWriter = new StreamWriter(pipeClient, new System.Text.UTF8Encoding(false));

        var stdin = Console.In;
        var stdout = Console.Out;

        var tasks = new Task<char[]>[2]
        {
            ReadHeaderDelimitedAsync(pipeReader, false),
            ReadHeaderDelimitedAsync(stdin, true)
        };

        while (true)
        {
            var doneIdx = Task.WaitAny(tasks);

            var bytesRead = tasks[doneIdx].Result;
            if (doneIdx == 0)
            {
                // pipe in -> stdout
                if (bytesRead.Length == 0)
                {
                    // pipe was closed
                    break;
                }

                stdout.Write(bytesRead, 0, bytesRead.Length);
                stdout.Flush();
                tasks[doneIdx] = ReadHeaderDelimitedAsync(pipeReader, false);
            }
            else
            {
                // stdin -> pipe out
                pipeWriter.Write(bytesRead, 0, bytesRead.Length);
                pipeWriter.Flush();
                tasks[doneIdx] = ReadHeaderDelimitedAsync(stdin, true);
            }
        }
    }

    private static async Task<char[]> ReadHeaderDelimitedAsync(TextReader reader, bool isInput)
    {
        // Assigning new tasks with this function blocks the thread
        // unless this is awaited first.
        await Task.Yield();

        string name = isInput ? "stdin -> pipe" : "pipe -> stdout";

        string line = null;
        while (line == null) {
            line = await reader.ReadLineAsync();
        }

        var colonPos = line.IndexOf(":");
        if (colonPos == -1)
        {
            // throw new Exception("Did not find colon inside header");
            return new char[0];
        }

        var headerName = line.Substring(0, colonPos);
        var headerContent = line.Substring(colonPos + 1);

        if (headerName != "Content-Length")
        {
            // throw new Exception("Expected Content-Length header");
            return new char[0];
        }
        if (headerContent.Length > 20)
        {
            // throw new Exception("Content-Length header too large");
            return new char[0];
        }

        int contentLength;
        try 
        {
            contentLength = int.Parse(headerContent);
        }
        catch (Exception)
        {
            return new char[0];
        }

        var buffer = new char[line.Length + contentLength + 4];
        var c = 0;
        for (var i = 0; i < line.Length; i++)
        {
            buffer[c++] = line[i];
        }
        buffer[c++] = '\r';
        buffer[c++] = '\n';
        buffer[c++] = '\r';
        buffer[c++] = '\n';

        await reader.ReadLineAsync();

        var bytesRead = await reader.ReadBlockAsync(buffer, c, contentLength);

        return buffer;
    }
}
"@

Add-Type -TypeDefinition $Source -Language CSharp

[StdPipe]::RouteToPipe($ServerName, $PipeName)
