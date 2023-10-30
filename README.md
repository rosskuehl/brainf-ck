# brainf#ck

Brainf#ck is a toy compiler that reads in [brainfuck](https://esolangs.org/wiki/Brainfuck) source files and uses [AssemblyBuilder](https://learn.microsoft.com/en-us/dotnet/api/system.reflection.emit.assemblybuilder?view=net-8.0) to generate dynamic assemblies and execute their code during the same application run.

### Example usage

```bash
dotnet run scripts/hello.bf
Hello World!

dotnet run -- --size double scripts/cellsize.bf
16 bit cells
```