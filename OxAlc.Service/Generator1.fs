module Generator1
    open System
    open System.Collections.Generic
    open Typedefinitions
    open System.Reflection

    let rec mapToIlTypes lambda_type =
        match lambda_type with
        | Atom "number" -> typeof<float>
        | Atom "bool" -> typeof<bool>
        | Arrow (left_node, right_node) 
            -> typeof<Func<_,_>>.MakeGenericType(mapToIlTypes left_node, mapToIlTypes right_node)

    let SymbolTable = new Dictionary<_,_>()

    let IlGenerator = 
        let asmName = new AssemblyName("Generator")
        let asmBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(asmName, AssemblyBuilderAccess.RunAndSave)
        let modBuilder = asmBuilder.DefineDynamicModule("Generator", "Generator.dll")
        let typBuilder = modBuilder.DefineType("Generator", TypeAttributes.Public)
        let methBuilder = typBuilder.DefineMethod("Main", Reflect.MethodAttributes.Static, typeof(System.Void), System.Type.EmptyTypes); 
        methBuilder.GetILGenerator()

    let rec GenerateIl (statement : Statement) = 
        match statement with
        | Value lit -> 
            match lit with
            | Bool b -> IlGenerator.Emit(OpCodes.Ldc_I4, if b then 1 else 0)
            | Variable v -> IlGenerator.Emit(OpCodes.Ldc_I4, v)
            | _ -> failwith "Not implemented"
        | Bind (Identifier(name), type_expr, value, body) -> 
            this.symbolTable[name] <- this.il.DeclareLocal(mapToIlTypes type_expr)
            
            GenerateIl value

            IlGenerator.Emit(OpCodes.Stloc, SymbolTable.[name])
            GenerateIl body

