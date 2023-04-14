using System.Diagnostics;
using Mono.Cecil;
using Mono.Cecil.Rocks;
using Mono.Cecil.Cil;
using Fody;

namespace Pursuit.Fody;

public class ModuleWeaver : BaseModuleWeaver
{
    public override void Execute()
    {
        foreach (var type in ModuleDefinition.GetAllTypes())
        {
            foreach (var method in type.Methods)
            {
                var state = 0;
                var msil = (string?)null;

                foreach (var instruction in method.Body.Instructions)
                {
                    switch (instruction.OpCode.Code)
                    {
                        case Code.Nop: continue;
                        case Code.Ldstr:
                            if (state == 0)
                            {
                                msil = (string)instruction.Operand;
                                state = 1;
                            }
                            else
                            {
                                state = -1;
                            }

                            break;
                        case Code.Call:
                            if (state == 1)
                            {
                                var methodRef = (MethodReference)instruction.Operand;
                                if (methodRef.FullName == "System.Void Pursuit.InlineIL::Do(System.String)")
                                {
                                    state = 2;   
                                }
                                else
                                {
                                    state = -1;
                                }
                            }
                            else
                            {
                                state = -1;
                            }
                            
                            break;
                        case Code.Ret:
                            if (state == 2)
                            {
                                state = 3;
                            }
                            else
                            {
                                state = -1;
                            }

                            break;
                        default:
                            state = -1;
                            break;
                    }

                    if (state == -1) break;
                }

                if (state != 3) continue;

                var parsedMsil = IlParser.parseCilCode(msil).ToArray();
                
                method.Body.Instructions.Clear();

                foreach (var newInsn in parsedMsil)
                {
                    if (newInsn.TryLdstr(out var str))
                    {
                        method.Body.Instructions.Add(Instruction.Create(OpCodes.Ldstr, str));
                    }
                    else if (newInsn.TryCall(out var asm, out var tpeName, out var methn, out var rettpe, out var argv))
                    {
                        var resolvedAsm = ModuleDefinition.AssemblyResolver.Resolve(new AssemblyNameReference(asm, null));
                        var tpe = resolvedAsm.Modules[0].ExportedTypes.First(x => x.FullName == tpeName).Resolve();
                        
                        // todo real signature matching
                        var targetMeth = tpe.Methods.First(x => x.Name == methn && x.ReturnType.Resolve() == TypeSystem.VoidDefinition && x.Parameters.Count == 1 && x.Parameters[0].ParameterType.Resolve() == TypeSystem.StringDefinition);
                        
                        var importedMeth = ModuleDefinition.ImportReference(targetMeth);
                        method.Body.Instructions.Add(Instruction.Create(OpCodes.Call, importedMeth));
                    } 
                    else if (newInsn.TryRet())
                    {
                        method.Body.Instructions.Add(Instruction.Create(OpCodes.Ret));
                    }
                }
            }
        }
    }

    public override IEnumerable<string> GetAssembliesForScanning()
    {
        yield return "netstandard";
        yield return "mscorlib";
    }
    
    public override bool ShouldCleanReference => true;
}