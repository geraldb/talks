# GoReleaser Talk Notes

- Vienna Go (Lang) Meetup March 2023  @ fiskaly



## How-To Cross-Compile Go Programs for Window, Mac OS, and Linux

Cross-Compile == Build Go Programs / Applications for Different Operation Systems (`GOOS`) and
(Processor) Architectures (`GOARCH`) e.g. build Mac OS on Windows or  Windows on Linux etc.


Q: Cross-Compile!?

> Programs written in Go can easily be compiled for a wide variety of target operating systems
> such as Windows, Mac OS, and Linux
> by using the `GOOS` and `GOARCH` environmental variables.


Q: What is GOOS and GOARCH?

> `GOOS` refers to the operating system (Linux, Windows, BSD, etc.),
> while `GOARCH` refers to the (processor) architecture (386, amd64, arm64, etc.) to build for.


Q: How-to find out your GOOS and GOARCH values?

      $ go env GOOS
      windows
      $ go env GOARCH
      amd64
      $ go env GOOS GOARCH
      windows
      amd64

or

      $ go env

resulting in (sorted a-z):

    ..
    set GOARCH=amd64
    ...
    set GOOS=windows
    ...

 or

     $ go version
     go version go1.18 windows/amd64


Q: What values for GOOS and GOARCH possible?

      $ go tool dist list

resulting in:

```
aix/ppc64
android/386
android/amd64
android/arm
android/arm64
darwin/amd64
darwin/arm64
dragonfly/amd64
freebsd/386
freebsd/amd64
freebsd/arm
freebsd/arm64
illumos/amd64
ios/amd64
ios/arm64
js/wasm
linux/386
linux/amd64
linux/arm
linux/arm64
linux/mips
linux/mips64
linux/mips64le
linux/mipsle
linux/ppc64
linux/ppc64le
linux/riscv64
linux/s390x
netbsd/386
netbsd/amd64
netbsd/arm
netbsd/arm64
openbsd/386
openbsd/amd64
openbsd/arm
openbsd/arm64
openbsd/mips64
plan9/386
plan9/amd64
plan9/arm
solaris/amd64
windows/386
windows/amd64
windows/arm
windows/arm64
```



Coding Example - hello.go:

``` go
package main

import(
       "fmt"
       "runtime"
)

func main() {
    fmt.Printf( "OS: %s\nArchitecture: %s\n",
         runtime.GOOS,
         runtime.GOARCH)
}
```

build  (64-bit binary)

      $ go build hello.go

run

     $ hello
     OS: windows
     Architecture: amd64

build (32-bit binary)

      $ set GOOS=windows
      $ set GOARCH=386
      $ go build hello.go

run

      $ hello
      OS: windows
      Architecture: 386

build (linux binary on windows)

     $ set GOOS=linux
     $ set GOARCH=386
     $ go build hello.go

run

     $ hello

     > This version of hello is not compatible with the version of Windows you're running.
     > Check your computer's system information and then contact the software publisher.


Note: You CAN run the the binary / executable
only on the target platform (e.g. on Window, Linux, Mac OS, etc.) -
out the other end only runs on OS X, and cannot be run on


Cross-compile in unix shell:

     $ GOOS=linux GOARCH=386 go build hello.go

     $ GOOS=darwin GOARCH=arm64 go build hello.go
     ..


## Artbase Server Binary  Case Study

see <https://github.com/pixelartexchange/artbase.server>


Download Releases (Binaries) On Github

see <https://github.com/pixelartexchange/artbase.server/releases>

Example:

- `artbase_1.0.0_linux_64bit.tar.gz` - 2.84 MB
- `artbase_1.0.0_linux_armv6.tar.gz` - 2.66 MB
- `artbase_1.0.0_macos_64bit.tar.gz` - 2.93 MB
- `artbase_1.0.0_macos_arm64.tar.gz` - 2.82 MB
- `artbase_1.0.0_windows_64bit.zip` - 2.82 MB



Automate, Automate, Automate

via GitHub Actions & GoReleaser

GoReleaser (Freemium with Pro version) -
Release Go projects as fast and easily as possible!

see <https://goreleaser.com/>


How-To Use

Step 1:  .gorealser.yml  Configuration

see <https://github.com/pixelartexchange/artbase.server/blob/master/.goreleaser.yml>



Step 2:  .github/workflows/release_build.yml      (Git Action / Workflow Configuration)

see <https://github.com/pixelartexchange/artbase.server/blob/master/.github/workflows/release_build.yml>


Step 3:   git tag  (triggers goreleaser)

see <https://github.com/pixelartexchange/artbase.server/tags>

    $ git tag v0.0.5
    $ git tag v1.0.0

only "local" - to trigger goreleaser push tags to github (upstream)

    $  git push origin --tags
      or
    $  git push origin v0.0.5
    $  git push origin v1.0.0



That's it.

## Questions? Comments?

