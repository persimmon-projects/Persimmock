#r @"packages/build/FAKE/tools/FakeLib.dll"
open Fake
open Fake.Git
open Fake.ReleaseNotesHelper
open System.IO

let outDir = "bin"

let configuration = getBuildParamOrDefault "configuration" "Release"

let project = "Persimmock"

let solutionFile  = "Persimmock.sln"

let gitOwner = "persimmon-projects"
let gitHome = "https://github.com/" + gitOwner

let gitName = "Persimmock"

let gitRaw = environVarOrDefault "gitRaw" "https://raw.github.com/persimmon-projects"

let release = LoadReleaseNotes "RELEASE_NOTES.md"

Target "Clean" (fun _ ->
  CleanDirs [outDir; "temp"]
  !! ("./src/**/bin" @@ configuration)
  |> CleanDirs
)

Target "Build" (fun _ ->

  DotNetCli.Restore (fun p ->
    { p with
        Project = solutionFile
    }
  )

  !! solutionFile
  |> MSBuild "" "Rebuild" [ ("Platform", "Any CPU"); ("Configuration", configuration) ]
  |> ignore
)

Target "RunTests" (fun _ ->
  DotNetCli.Test (fun p -> { p with Project = "./tests/Persimmock.Tests" })
)

Target "NuGet" (fun _ ->
  Paket.Pack(fun p ->
    { p with
        OutputPath = outDir
        Version = release.NugetVersion
        ReleaseNotes = toLines release.Notes})
)

Target "PublishNuget" (fun _ ->
  Paket.Push(fun p -> { p with WorkingDir = outDir })
)

#load "paket-files/build/fsharp/FAKE/modules/Octokit/Octokit.fsx"
open Octokit

Target "Release" (fun _ ->
  StageAll ""
  Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
  Branches.pushBranch "" "origin" "master"

  Branches.tag "" release.NugetVersion
  Branches.pushTag "" "origin" release.NugetVersion

  // release on github
  createClient (getBuildParamOrDefault "github-user" "") (getBuildParamOrDefault "github-pw" "")
  |> createDraft gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
  // TODO: |> uploadFile "PATH_TO_FILE"
  |> releaseDraft
  |> Async.RunSynchronously
)

Target "All" DoNothing

"Clean"
  ==> "Build"
  ==> "RunTests"
  ==> "All"

"All"
  ==> "NuGet"

"NuGet"
  ==> "PublishNuget"
  ==> "Release"

RunTargetOrDefault "All"
