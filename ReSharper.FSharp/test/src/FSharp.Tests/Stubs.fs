namespace JetBrains.ReSharper.Plugins.FSharp.Tests.Common

open JetBrains.Application
open JetBrains.Application.Components
open JetBrains.ProjectModel
open JetBrains.ReSharper.Plugins.FSharp.Common
open JetBrains.ReSharper.Plugins.FSharp.Common.Checker
open JetBrains.ReSharper.Plugins.FSharp.ProjectModel
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Fsi
open JetBrains.Util

[<SolutionComponent>]
type FsiSessionsHostStub() = 
    interface IHideImplementation<FsiSessionsHost>

[<SolutionComponent>]
type FSharpProjectOptionsBuilderStub() = 
    interface IHideImplementation<FSharpProjectOptionsBuilder>

[<ShellComponent>]
type FSharpFileServiceStub() =
    interface IHideImplementation<FSharpFileService>

    interface IFSharpFileService with
        member x.IsScratchFile(_) = false
        member x.IsScriptLike(_) = false

[<ShellComponent>]
type FsiDetectorStub() =
    interface IHideImplementation<FsiDetector>

    interface IFsiDetector with
        member x.GetSystemFsiDirectoryPath() = FileSystemPath.Empty 

/// Used to add assemblies to R# subplatfrom at runtime
type AddAssembliesToSubplatform() =
    let _ = FsiSessionsHostStub
    let _ = FSharpProjectLoadTargetsAnalyzer()
