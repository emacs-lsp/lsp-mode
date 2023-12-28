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

        var pipeBuffer = new char[512];
        var stdBuffer = new char[512];

        var tasks = new Task<int>[2]
        {
            pipeReader.ReadAsync(pipeBuffer, 0, pipeBuffer.Length),
            ReadFromStdinAsync(stdBuffer, 0, stdBuffer.Length)
        };

        while (true)
        {
            var doneIdx = Task.WaitAny(tasks);

            var bytesRead = tasks[doneIdx].Result;
            if (doneIdx == 0)
            {//Input from pipe
             if (bytesRead == 0)
             {// If the pipe has closed out on us, punk out
                 break;
             }

             Console.Out.Write(pipeBuffer, 0, bytesRead);
             Console.Out.Flush();
             tasks[doneIdx] = pipeReader.ReadAsync(pipeBuffer, 0, pipeBuffer.Length);
            }
            else
            {//Input from stdin
             pipeWriter.Write(stdBuffer, 0, bytesRead);
             pipeWriter.Flush();
             tasks[doneIdx] = ReadFromStdinAsync(stdBuffer, 0, stdBuffer.Length);
            }
        }
    }

    private static Task<int> ReadFromStdinAsync(char[] buffer, int offset, int len)
    {
        return Task.Run(() =>
                        {
                            return Console.In.Read(buffer, offset, len);
                        });
    }
}
"@

Add-Type -TypeDefinition $Source -Language CSharp

[StdPipe]::RouteToPipe($ServerName, $PipeName)
