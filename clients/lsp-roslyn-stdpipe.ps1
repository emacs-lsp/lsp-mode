param (
    [string]$ServerName = ".",
    [Parameter(Mandatory=$true)][string]$PipeName
)

# Use named pipe as stdin/out

$Source = @"
using System;
using System.Text;
using System.IO;
using System.IO.Pipes;
using System.Threading.Tasks;

public static class StdPipe
{
    public static void RouteToPipe(string pipeServer, string pipeName)
    {
        var pipeClient = new NamedPipeClientStream(pipeServer, pipeName, PipeDirection.InOut, PipeOptions.Asynchronous);
        pipeClient.Connect();

        var pipeReader = new BufferedStream(pipeClient);
        var pipeWriter = new BufferedStream(pipeClient);

        var stdin = new BufferedStream(Console.OpenStandardInput());
        var stdout = new BufferedStream(Console.OpenStandardOutput());

        var tasks = new Task<byte[]>[2]
        {
            ReadHeaderDelimitedAsync(pipeReader),
            ReadHeaderDelimitedAsync(stdin)
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
                tasks[doneIdx] = ReadHeaderDelimitedAsync(pipeReader);
            }
            else
            {
                // stdin -> pipe out
                pipeWriter.Write(bytesRead, 0, bytesRead.Length);
                pipeWriter.Flush();
                tasks[doneIdx] = ReadHeaderDelimitedAsync(stdin);
            }
        }
    }

    private static async Task<byte[]> ReadHeaderDelimitedAsync(Stream stream)
    {
        // Assigning new tasks with this function blocks the thread
        // unless this is awaited first.
        await Task.Yield();

        var idx = 0;
        var header = new byte[64];
        int b = 0;
        do
        {
            var bytesRead = await stream.ReadAsync(header, idx, 1);
            if (bytesRead == 0)
                continue;
            b = header[idx++];
        } while (b != '\r');

        var colonPos = Array.IndexOf(header, (byte)':');
        if (colonPos == -1)
        {
            return new byte[0];
        }

        var headerName = new byte[colonPos];
        Array.Copy(header, headerName, colonPos);
        var headerContent = new byte[idx - colonPos - 1];
        Array.Copy(header, colonPos + 2, headerContent, 0, headerContent.Length - 2);

        if (Encoding.ASCII.GetString(headerName) != "Content-Length")
        {
            return new byte[0];
        }
        if (headerContent.Length > 20)
        {
            return new byte[0];
        }
        int contentLength;
        try
        {
            contentLength = int.Parse(Encoding.ASCII.GetString(headerContent));
        }
        catch (Exception)
        {
            return new byte[0];
        }

        var buffer = new byte[contentLength + idx + 3];
        var c = 0;
        for (var i = 0; i < idx; i++)
        {
            buffer[c++] = header[i];
        }

        // LF, CRLF
        var bytesToRead = contentLength + 3;
        while (bytesToRead > 0)
        {
            var bytesRead = await stream.ReadAsync(buffer, c, bytesToRead);
            bytesToRead -= bytesRead;
            c += bytesRead;
        }

        return buffer;
    }
}
"@

Add-Type -TypeDefinition $Source -Language CSharp

try {
    [StdPipe]::RouteToPipe($ServerName, $PipeName)
} catch [System.AggregateException] {
    Write-Error $error[0].exception.innerexception
    throw $error[0].exception.innerexception
}
