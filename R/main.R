
#' @import jmvcore
NULL

jmcPath <- function() {
    paste0('"', system.file('node_modules', 'jamovi-compiler', 'index.js', package='jmvtools'), '"')
}

isWindows <- function() {
    Sys.info()['sysname'] == 'Windows'
}

isLinux <- function() {
    Sys.info()['sysname'] == 'Linux'
}

#' The current version
#'
#' returns the current version of jmvtools
#'
#' @export
version <- function() {
    lines <- readLines(system.file('DESCRIPTION', package='jmvtools'))
    version <- lines[grepl('^Version:', lines)]
    version <- substring(version, 10)
    version
}

extraArgs <- function(home=NULL, rhome=NULL, debug=FALSE) {
    args <- character()
    if (is.null(home))
        home <- getOption('jamovi_home')
    if (is.null(home) && isLinux())
        home <- 'flatpak'
    if ( ! is.null(home) && isWindows())
        home <- paste0('"', home, '"')
    if ( ! is.null(home))
        args <- c(args, '--home', home)
    if ( ! isWindows() && ! is.null(rhome))
        args <- c(args, '--rpath', rhome)
    if (debug)
        args <- c(args, '--debug')
    args
}

#' Check that jmvtools is able to find jamovi
#'
#' @param home path to a local jamovi installation
#' @importFrom node node
#' @export
check <- function(home=NULL) {

    exe <- node()
    jmc <- jmcPath()
    xArgs <- extraArgs(home)
    args <- c(jmc, '--check', xArgs)
    system2(exe, args, wait=TRUE)
}

#' Build and install a local jamovi module into jamovi
#'
#' @param pkg path to a local directory containing the module source
#' @inheritParams check
#' @importFrom node node
#' @export
install <- function(pkg='.', home=NULL, debug=FALSE) {

    exe <- node()
    jmc <- jmcPath()
    pkg <- paste0('"', pkg, '"')
    rhome <- paste0('"', R.home(component='bin'), '"')

    xArgs <- extraArgs(home, rhome, debug)
    args <- c(jmc, '--install', pkg, xArgs)

    system2(exe, args, wait=TRUE)
}

#' Prepare a jamovi source module
#'
#' @inheritParams install
#' @importFrom node node
#' @export
prepare <- function(pkg='.', home=NULL) {

    exe <- node()
    jmc <- jmcPath()
    pkg <- paste0('"', pkg, '"')
    rhome <- paste0('"', R.home(component='bin'), '"')

    xArgs <- extraArgs(home, rhome)
    args <- c(jmc, '--prepare', pkg, xArgs)

    system2(exe, args, wait=TRUE)
}

#' Create translations
#'
#' @inheritParams install
#' @importFrom node node
#' @export
i18nCreate <- function(code, pkg='.', home=NULL) {

    exe <- node()
    jmc <- jmcPath()
    pkg <- paste0('"', pkg, '"')

    xArgs <- extraArgs(home)
    args <- c(jmc, '--i18n', pkg, '--create', code, xArgs)

    system2(exe, args, wait=TRUE)
}

#' Update translations
#'
#' @inheritParams install
#' @importFrom node node
#' @export
i18nUpdate <- function(code='*', pkg='.', home=NULL, debug=FALSE) {

    exe <- node()
    jmc <- jmcPath()
    pkg <- paste0('"', pkg, '"')

    xArgs <- extraArgs(home)
    args <- c(jmc, '--i18n', pkg, '--update')
    if (code != '*')
        args <- c(args, code)
    args <- c(args, xArgs)
    if (debug)
        args <- c(args, '--verbose')

    system2(exe, args, wait=TRUE)
}

#' Create an empty jamovi module
#'
#' Creates an empty jamovi module. Astute observers will notice that empty
#' jamovi modules are the same as empty R packages.
#'
#' @param path location to create the new module (the name of the module is inferred from the path)
#' @export
create <- function(path='.', home=NULL, gitignore=TRUE) {

    path <- normalizePath(path, winslash='/', mustWork=FALSE)
    name <- basename(path)

    if (length(grep('^[a-zA-Z][a-zA-Z0-9]+$', name)) == 0)
        stop('Module names must be at least two characters long and consist only of letters and numbers')

    parentDir <- dirname(path)
    if ( ! file.exists(parentDir))
        stop(paste0('Parent directory \'', parentDir, '\' does not exist'))

    if (file.exists(path)) {
        if (length(dir(path)) > 0)
            stop('Directory already exists and is not empty', call.=FALSE)
    }
    else {
        dir.create(path)
    }

    dir.create(file.path(path, 'R'))
    dir.create(file.path(path, 'jamovi'))

    DESCRIPTION_path <- system.file('templates', 'DESCRIPTION', package='jmvtools', mustWork=TRUE)
    NAMESPACE_path   <- system.file('templates', 'NAMESPACE',   package='jmvtools', mustWork=TRUE)

    DESCRIPTION_content <- paste0(readLines(DESCRIPTION_path, encoding='UTF-8'), collapse='\n')
    NAMESPACE_content   <- paste0(readLines(NAMESPACE_path,   encoding='UTF-8'), collapse='\n')

    DESCRIPTION_content <- gsub('\\$NAME', name, DESCRIPTION_content)

    DESCRIPTION_path <- file.path(path, 'DESCRIPTION')
    NAMESPACE_path   <- file.path(path, 'NAMESPACE')

    writeLines(DESCRIPTION_content, DESCRIPTION_path)
    writeLines(NAMESPACE_content,   NAMESPACE_path)

    if (gitignore) {
        GITIGNORE_path <- system.file('templates', 'gitignore', package='jmvtools', mustWork=TRUE)
        GITIGNORE_content <- paste0(readLines(GITIGNORE_path, encoding='UTF-8'), collapse='\n')
        GITIGNORE_path <- file.path(path, '.gitignore')
        writeLines(GITIGNORE_content, GITIGNORE_path)
    }

    prepare(path, home)
}

#' Adds a new analysis to a jamovi module
#'
#' @param name the name for the new analysis
#' @param title the title for the new analysis
#' @inheritParams check
#' @export
addAnalysis <- function(name, title=name, path='.', home=NULL) {

    if ( ! is.character(name) && length(name) != 1)
        stop('title must be a string', call.=FALSE)

    if ( ! is.character(title) && length(name) != 1)
        stop('title must be a string', call.=FALSE)

    if (length(grep('^[a-zA-Z][a-zA-Z0-9]+$', name)) == 0)
        stop('Analysis names must be at least two characters long and consist only of letters and numbers')

    if ( ! file.exists(file.path(path, 'DESCRIPTION')))
        stop('path does not contain a DESCRITPION file, does not appear to be a package or module', call.=FALSE)

    path <- normalizePath(path, winslash='/', mustWork=FALSE)
    moduleName <- basename(path)

    jamoviPath <- file.path(path, 'jamovi')
    if ( ! dir.exists(jamoviPath))
        dir.create(jamoviPath)

    aYamlPath <- system.file('templates', 'a.yaml', package='jmvtools', mustWork=TRUE)
    rYamlPath <- system.file('templates', 'r.yaml', package='jmvtools', mustWork=TRUE)

    aYamlContent <- paste0(readLines(aYamlPath, encoding='UTF-8'), collapse='\n')
    rYamlContent <- paste0(readLines(rYamlPath, encoding='UTF-8'), collapse='\n')

    aYamlContent <- gsub('\\$NAME',  name,  aYamlContent)
    aYamlContent <- gsub('\\$TITLE', title, aYamlContent)
    aYamlContent <- gsub('\\$MODULE_NAME', moduleName, aYamlContent)
    rYamlContent <- gsub('\\$NAME',  name,  rYamlContent)
    rYamlContent <- gsub('\\$TITLE', title, rYamlContent)
    rYamlContent <- gsub('\\$MODULE_NAME', moduleName, rYamlContent)

    aYamlPath <- file.path(jamoviPath, paste0(tolower(name), '.a.yaml'))
    rYamlPath <- file.path(jamoviPath, paste0(tolower(name), '.r.yaml'))

    if (file.exists(aYamlPath))
        stop(paste0('analysis \'', name, '\' already exists'), call.=FALSE)

    writeLines(aYamlContent, aYamlPath)
    writeLines(rYamlContent, rYamlPath)

    prepare(path, home)
}
