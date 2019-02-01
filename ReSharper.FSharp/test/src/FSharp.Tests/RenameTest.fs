namespace JetBrains.ReSharper.Plugins.FSharp.Tests.Features.Refactorings

open JetBrains.ReSharper.FeaturesTestFramework.Refactorings
open JetBrains.ReSharper.Plugins.FSharp.ProjectModel.ProjectProperties
open JetBrains.ReSharper.Plugins.FSharp.Tests
open NUnit.Framework

[<FSharpTest>]
type FSharpRenameTest() =
    inherit RenameTestBase()

    override x.RelativeTestDataPath = "features/refactorings/rename"

    override x.GetProjectProperties(targetFrameworkIds, flavours) =
        FSharpProjectPropertiesFactory.CreateProjectProperties(targetFrameworkIds)

    [<Test>] member x.``Inline - Declaration``() = x.DoNamedTest()
    [<Test>] member x.``Inline - Usage``() = x.DoNamedTest()
    [<Test>] member x.``Inline - Multiple usages``() = x.DoNamedTest()
    [<Test>] member x.``Inline - member self id``() = x.DoNamedTest()
    [<Test>] member x.``Inline - ctor self id``() = x.DoNamedTest()
    [<Test>] member x.``Inline - simple binding``() = x.DoNamedTest()

    [<Test>] member x.``Inline - synPat or 1``() = x.DoNamedTest()
    [<Test>] member x.``Inline - synPat or 2``() = x.DoNamedTest()
    [<Test>] member x.``Inline - synPat or 3``() = x.DoNamedTest()

    [<Test>] member x.``Module binding - Simple pattern, declaration``() = x.DoNamedTest()
    [<Test>] member x.``Module binding - Simple pattern, reference``() = x.DoNamedTest()
    [<Test>] member x.``Module binding - Function``() = x.DoNamedTest()
    [<Test>] member x.``Module binding - Tuple``() = x.DoNamedTest()
    [<Test>] member x.``Module binding - Named pat 01 - id``() = x.DoNamedTest()
    [<Test>] member x.``Module binding - Named pat 02 - pat``() = x.DoNamedTest()
    [<Test>] member x.``Module binding - Named pat 03 - nested pat``() = x.DoNamedTest()

    [<Test>] member x.``Module binding - synPat or``() = x.DoNamedTest()

    [<Test>] member x.``Module binding - nested synPat or 1``() = x.DoNamedTest()
    [<Test>] member x.``Module binding - nested synPat or 2``() = x.DoNamedTest()
    [<Test>] member x.``Module binding - nested synPat or 3``() = x.DoNamedTest()

    [<Test>] member x.``Types - Record 01``() = x.DoNamedTest()
    [<Test>] member x.``Types - Record 02 - Struct``() = x.DoNamedTest()

    [<Test>] member x.``Types - Enum 01``() = x.DoNamedTest()
    [<Test>] member x.``Types - Struct 01``() = x.DoNamedTest()
    [<Test>] member x.``Types - Interface 01``() = x.DoNamedTest()

    [<Test>] member x.``Types - Exception 01``() = x.DoNamedTest() // todo: make upper case name
