# Micro Maven Resolver

This uses the maven-resolver-supplier and exposes a few functions from that very big api.

## Coordinate specification

```<groupId>:<artifactId>[:<extension>[:<classifier>]]:<version>```

eg:

```
# implicit extension=jar, classifier=
org.openapitools:openapi-generator-cli:7.13.0

# extension=jar, classifier=
org.openapitools:openapi-generator-cli:jar:7.13.0
 
# extension=jar, classifier=sources
org.openapitools:openapi-generator-cli:jar:sources:7.13.0 
```

The version specification allows us to have two meta version (LATEST, RELEASE).
We accept lowercase variants of these meta versions as well.

eg:

```
# implicit extension=jar, classifier=
org.openapitools:openapi-generator-cli:latest
org.openapitools:openapi-generator-cli:LATEST
org.openapitools:openapi-generator-cli:release
org.openapitools:openapi-generator-cli:RELEASE

# extension=jar, classifier=
org.openapitools:openapi-generator-cli:jar:latest
org.openapitools:openapi-generator-cli:jar:LATEST
org.openapitools:openapi-generator-cli:jar:release
org.openapitools:openapi-generator-cli:jar:RELEASE
 
# extension=jar, classifier=sources
org.openapitools:openapi-generator-cli:jar:sources:latest 
org.openapitools:openapi-generator-cli:jar:sources:LATEST 
org.openapitools:openapi-generator-cli:jar:sources:release 
org.openapitools:openapi-generator-cli:jar:sources:RELEASE 
```

## versions

given a module `"<groupId>:<artifactId>"` list all available versions.
Supports a few formats which are printed to standard out.

* JSON
* Table (default)
* latest
* release

## resolve

Downloads the artifact with the coordinate specification.
The resolved file is printed to standard out.

Can optionally extract the artifact to a folder.
Will always download the file to the maven local repository.

The default location is `$HOME/.m2/repository`.

This location cannot currently be overridden.

## resolve-version

resolves the version from artifact with the coordinate specification.
The resolved version is printed to standard out

This is mostly useful when you have a snapshot version or one of the meta (LATEST, RELEASE) versions.

eg:
```resolve-version org.openapitools:openapi-generator-cli:latest```
```resolve-version org.openapitools:openapi-generator-cli:release```

## deploy

Deploys a file with the given coordinate specification

Generates a POM file, and handles maven-metadata.xml correctly.
Now we can query the available versions for the supplied artifact file.

## Installation

### Download using mise

Get mise [here](https://mise.jdx.dev/).

```
mise use "ubi:sikt-no/micro-maven-resolver@latest"
```

### Manual installation via download of files from github releases.

If you download the file on macos using a browser, make sure you allow it to run by running

`xattr -d com.apple.quarantine /path/to/downloaded-file`

You also have to make sure you make this executable

`chmod 755 /path/to/downloaded-file`
