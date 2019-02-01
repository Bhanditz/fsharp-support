namespace rec JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Refactorings.Rename

open System
open JetBrains.Application
open JetBrains.ReSharper.Feature.Services.Refactorings.Specific.Rename
open JetBrains.ReSharper.Plugins.FSharp.Common.Util
open JetBrains.ReSharper.Plugins.FSharp.Psi
open JetBrains.ReSharper.Plugins.FSharp.Psi.Impl.Cache2
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Naming.Impl
open JetBrains.ReSharper.Psi.Resolve
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Refactorings.Rename
open JetBrains.Util
open Microsoft.FSharp.Compiler.SourceCodeServices

[<AutoOpen>]
module Util =
    type IRenameWorkflow with
        member x.RenameWorkflow =
            match x with
            | :? RenameWorkflow as workflow -> workflow
            | _ -> failwithf "Got workflow: %O" x

        member x.RenameDataModel =
            x.RenameWorkflow.DataModel

        member x.FSharpRenameModel =
            x.RenameDataModel.FSharpRenameModel

    type RenameDataModel with
        member x.FSharpRenameModel =
            x.Model :?> FSharpCustomRenameModel


type FSharpCustomRenameModel(declaredElement, reference, lifetime, changeNameKind: ChangeNameKind) =
    inherit ClrCustomRenameModel(declaredElement, reference, lifetime)

    member x.ChangeNameKind = changeNameKind


type FSharpAtomicRename(declaredElement, newName, doNotShowBindingConflicts) =
    inherit AtomicRename(declaredElement, newName, doNotShowBindingConflicts)

    override x.SetName(declaration, renameRefactoring) =
        match declaration with
        | :? IFSharpDeclaration as fsDeclaration ->
            fsDeclaration.SetName(x.NewName, renameRefactoring.Workflow.FSharpRenameModel.ChangeNameKind)
        | declaration -> failwithf "Got declaration: %O" declaration


[<Language(typeof<FSharpLanguage>)>]
type FSharpRenameHelper() =
    inherit RenameHelperBase()

    override x.IsLanguageSupported = true

    override x.IsCheckResolvedTo(newReference: IReference, newDeclaredElement: IDeclaredElement) =
        newDeclaredElement :? IFSharpDeclaredElement ||
        base.IsCheckResolvedTo(newReference, newDeclaredElement)

    override x.IsLocalRename(element: IDeclaredElement) = element :? ILocalDeclaration
    override x.CheckLocalRenameSameDocument(element: IDeclaredElement) = x.IsLocalRename(element)

    override x.GetOptionsModel(declaredElement, reference, lifetime) =
        FSharpCustomRenameModel(declaredElement, reference, lifetime, (* todo *) ChangeNameKind.SourceName) :> _

    override x.GetInitialPage(workflow) =
        let dataModel = workflow.RenameDataModel
        let declaredElement = dataModel.InitialDeclaredElement

        match declaredElement.As<IFSharpDeclaredElement>() with
        | null -> failwithf "Got declared element: %O" declaredElement
        | fsDeclaredElement ->

        dataModel.InitialName <-
            match dataModel.FSharpRenameModel.ChangeNameKind with
            | ChangeNameKind.SourceName
            | ChangeNameKind.UseSingleName -> fsDeclaredElement.SourceName
            | _ -> fsDeclaredElement.ShortName

        null            


[<ShellFeaturePart>]
type FSharpAtomicRenamesFactory() =
    inherit AtomicRenamesFactory()

    override x.IsApplicable(element: IDeclaredElement) =
        element.PresentationLanguage.Is<FSharpLanguage>()

    override x.CheckRenameAvailability(element: IDeclaredElement) =
        match element with
        | :? ILocalDeclaration -> RenameAvailabilityCheckResult.CanBeRenamed
        | _ -> RenameAvailabilityCheckResult.CanBeRenamed // todo: do not commit

    override x.CreateAtomicRenames(declaredElement, newName, doNotAddBindingConflicts) =
        [| FSharpAtomicRename(declaredElement, newName, doNotAddBindingConflicts) :> AtomicRenameBase |] :> _


[<Language(typeof<FSharpLanguage>)>]
type FSharpNamingService(language: FSharpLanguage) =
    inherit NamingLanguageServiceBase(language)

    static let notAllowedInTypes =
        [| '.'; '+'; '$'; '&'; '['; ']'; '/'; '\\'; '*'; '\"'; '`' |]

    override x.MangleNameIfNecessary(name, _) =
        Keywords.QuoteIdentifierIfNeeded name

    member x.IsSourceValidName(declaration: IDeclaration, name: string) =
        if name.IsEmpty() then false else

        match declaration with
        | :? IUnionCaseDeclaration -> not (Char.IsLower(name.[0]) || not (Char.IsUpper(name.[0])))
        | :? ITypeDeclaration -> name.IndexOfAny(notAllowedInTypes) <> -1
        | _ -> not (startsWith "`" name || endsWith "`" name || name.ContainsNewLine() || name.Contains("``"))


[<FileRenameProvider>]
type RelatedFileRenameProvider() =
    interface IFileRenameProvider with
        member x.GetFileRenames(declaredElement, name) =
            match declaredElement.As<IModule>() with
            | null -> EmptyList.Instance :> _
            | moduleTypeElement ->

            let psiServices = moduleTypeElement.GetPsiServices()
            moduleTypeElement.GetDeclarations() |> Seq.choose (fun decl ->
                let alwaysRename =
                    match decl with
//                    | :? ITopLevelModuleDeclaration as decl -> decl.IsAnon
                    | _ -> false

                match decl.GetSourceFile().ToProjectFile() with
                | null -> None
                | projectFile ->
                    Some (FileRename(psiServices, projectFile, name, alwaysRename)))
