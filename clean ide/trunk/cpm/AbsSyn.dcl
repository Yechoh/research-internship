definition module AbsSyn

from System.FilePath import :: FilePath

/**
 * Datatypes
 */
:: CpmAction
  =  Project FilePath ProjectAction
  |  Module String ModuleAction
  |  Environment EnvironmentAction
  |  CpmMake
  |  CpmHelp

:: ProjectAction
  =  CreateProject
  |  ShowProject
  |  BuildProject Bool FilePath
  |  ProjectPath PathAction
  |  SetRelativeRoot String
  |  SetTarget String
  |  SetExec String
  |  ProjectHelp

:: PathAction
  =  AddPathAction String
  |  RemovePathAction Int
  |  ListPathsAction
  |  MovePathAction Int PathDirection
  |  PathHelp

:: PathDirection
  =  MovePathUp
  |  MovePathDown
  |  MovePathTop
  |  MovePathBottom

:: ModuleAction
  =  CreateModule ModuleType
  |  ModuleHelp

:: ModuleType
  =  ApplicationModule
  |  LibraryModule

:: EnvironmentAction
  =  ListEnvironments
  |  ImportEnvironment FilePath
  |  RemoveEnvironment String
  |  ShowEnvironment String
  |  ExportEnvironment String
  |  CreateEnvironment String
  |  RenameEnvironment String String
  |  SetEnvironmentCompiler String String
  |  SetEnvironmentCodeGen String String
  |  EnvironmentHelp
  // TODO: EnvironmentPaths, EnvironmentVersion, EnvironmentProcessor, Environment64BitProcessor
