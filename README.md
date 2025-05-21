# Micro Maven Resolver

This uses the maven-resolver-supplier and exposes 3 functions from that very big api.

## versions
given a module `"<groupId>:<artifactId>"` list all available versions.
Supports two formats.

* JSON
* Table (default)

## resolve
Downloads the artifact with the coordinate specification defined by maven artifact.

```<groupId>:<artifactId>[:<extension>[:<classifier>]]:<version>```

Can optionally extract the artifact to a folder.
Will always download the file to the maven local repository.

The default location is `$HOME/.m2/repository`.

This location cannot currently be overriden.

## upload
Uploads a file with the given coordinate specification

```<groupId>:<artifactId>[:<extension>[:<classifier>]]:<version>```

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
