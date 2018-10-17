#!/bin/sh

litps compile --file README.md; mv README.purs test/Integration.purs; pulp test --main Test.Integration
