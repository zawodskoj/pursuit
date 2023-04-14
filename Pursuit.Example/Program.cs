// See https://aka.ms/new-console-template for more information

using Pursuit;

Console.WriteLine("Hello, World!");
InlineMe.Test();

static class InlineMe
{
    public static void Test()
    {
        InlineIL.Do("""
            ldstr "wtf"
            call void [mscorlib]System.Console::WriteLine(string)
            ret
        """);
    }
}