module FsAutoComplete.CodeFix.CodeFixTest

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.Text.Range


let tryRangeOfNameOfNearestOuterBindingContainingPos (input: ParsedInput) pos =
  let tryGetIdentRangeFromBinding binding =
    match binding with
    | SynBinding(headPat=headPat) ->
        match headPat with
        | SynPat.LongIdent (longDotId=longIdentWithDots) ->
            Some longIdentWithDots.Range
        | SynPat.As (rhsPat=SynPat.Named (ident=ident; isThisVal=false))
        | SynPat.Named (ident, false, _, _) ->
            Some ident.idRange
        | _ ->
            None

  let rec walkBinding expr workingRange =
    match expr with

    // This lets us dive into subexpressions that may contain the binding we're after
    | SynExpr.Sequential (_, _, expr1, expr2, _) ->
        if rangeContainsPos expr1.Range pos then
            walkBinding expr1 workingRange
        else
            walkBinding expr2 workingRange

    | SynExpr.LetOrUse(bindings=bindings; body=bodyExpr) ->
        let potentialNestedRange =
            bindings
            |> List.tryFind (fun binding -> rangeContainsPos binding.RangeOfBindingWithRhs pos)
            |> Option.bind tryGetIdentRangeFromBinding
        match potentialNestedRange with
        | Some range ->
            walkBinding bodyExpr range
        | None ->
            walkBinding bodyExpr workingRange

    
    | _ ->
        Some workingRange

  SyntaxTraversal.Traverse(pos, input, { new SyntaxVisitorBase<_>() with
    override _.VisitExpr(_, _, defaultTraverse, expr) =                        
        defaultTraverse expr

    override _.VisitBinding(_path, defaultTraverse, binding) =
        match binding with
        | SynBinding(valData=SynValData (memberFlags=None); expr=expr) as b when rangeContainsPos b.RangeOfBindingWithRhs pos ->
            match tryGetIdentRangeFromBinding b with
            | Some range -> walkBinding expr range
            | None -> None
        | _ -> defaultTraverse binding })

/// in let binding
/// Generate parameter 'x'
 
/// in module and let binding
/// Generate value 'x'
/// Generate function 'x'
/// Generate let binding 'x' ?

/// a codefix that fixes ...
let fix (getParseResultsForFile: GetParseResultsForFile) (getRangeText: GetRangeText) : CodeFix =
  Run.ifDiagnosticByCode
    (Set.ofList [ "39" ])
    (fun diagnostic codeActionParams ->
        asyncResult {

          let fileName =
            codeActionParams.TextDocument.GetFilePath() |> normalizePath
  
          let errorRangeStart = protocolPosToPos diagnostic.Range.Start
          let! (tyres, _line, _) = getParseResultsForFile fileName errorRangeStart

          let! outerBindingRange =
            tryRangeOfNameOfNearestOuterBindingContainingPos tyres.GetParseResults.ParseTree errorRangeStart
            |> Result.ofOption (fun _ -> "No outer binding found at pos")

          // let! outerBindingRange =
          //   tyres.GetParseResults.TryRangeOfNameOfNearestOuterBindingContainingPos errorRangeStart
          //   |> Result.ofOption (fun _ -> "No outer binding found at pos")

          let lspOuterBindingRange = fcsRangeToLsp outerBindingRange
          let charAfterBindingNameRange = 
            { Start = lspOuterBindingRange.End
              End = { lspOuterBindingRange.End with Character = lspOuterBindingRange.End.Character + 1 } }
          let! charAfterBindingName = getRangeText fileName charAfterBindingNameRange

          let! parameterName = getRangeText fileName diagnostic.Range
          let newText = 
            if charAfterBindingName = " "
            then $" %s{parameterName}"
            else $" %s{parameterName} "  

          return
            [ { File = codeActionParams.TextDocument
                Title = $"Generate parameter '%s{parameterName}'."
                SourceDiagnostic = Some diagnostic
                Edits =
                  [| { Range = 
                         { Start = lspOuterBindingRange.End
                           End = lspOuterBindingRange.End }
                       NewText = newText } |]
                Kind = FixKind.Fix } ]
        })
