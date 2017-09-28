#!/usr/bin/env bash

set -e

echo "TRAVIS_BRANCH='$TRAVIS_BRANCH'"
echo "TRAVIS_PULL_REQUEST='$TRAVIS_PULL_REQUEST'"

# For now, obfuscate SNAPSHOTs from sbt's developers: https://github.com/sbt/sbt/issues/2687#issuecomment-236586241
if [ "$TRAVIS_PULL_REQUEST" == "false" ]; then
    if [ "$TRAVIS_BRANCH" == "develop" ]; then
        sbt \
            'set test in Test := {}' \
            'set resolvers in ThisBuild += Resolver.url("bintray-sbt-plugin-releases", url("http://dl.bintray.com/content/sbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)' \
            'set publishTo in ThisBuild := Option("artifactory-publish" at "https://broadinstitute.jfrog.io/broadinstitute/libs-release-local;build.timestamp=" + new java.util.Date().getTime)' \
            "set credentials in ThisBuild += Credentials(\"Artifactory Realm\", \"broadinstitute.jfrog.io\", \"${ARTIFACTORY_USERNAME}\", \"${ARTIFACTORY_PASSWORD}\")" \
            +publish 2>&1 | sed 's/.*set credentials.*/REDACTED LOG LINE/'

    elif [[ "$TRAVIS_BRANCH" =~ ^[0-9\.]+_hotfix$ ]]; then
        sbt \
            'set test in Test := {}' \
            'set git.gitUncommittedChanges in ThisBuild := false' \
            'set resolvers in ThisBuild += Resolver.url("bintray-sbt-plugin-releases", url("http://dl.bintray.com/content/sbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)' \
            'set publishTo in ThisBuild := Option("artifactory-publish" at "https://broadinstitute.jfrog.io/broadinstitute/libs-release-local;build.timestamp=" + new java.util.Date().getTime)' \
            "set credentials in ThisBuild += Credentials(\"Artifactory Realm\", \"broadinstitute.jfrog.io\", \"${ARTIFACTORY_USERNAME}\", \"${ARTIFACTORY_PASSWORD}\")" \
            -Dproject.isSnapshot=false +publish 2>&1 | sed 's/.*set credentials.*/REDACTED LOG LINE/'

    fi
fi
