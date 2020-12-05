# morphala
Word morphology library for Scala

morphala is being used to generate Spanish word exceptional forms in [extjwnl-data-mcr30-2016](https://github.com/lingeringsocket/extjwnl-data-mcr30-2016).

Test Coverage
-------------

morphala runs coverage tests against the following resources:

* [Fred Jehle's Conjugated Spanish Verb Database](https://github.com/ghidinelli/fred-jehle-spanish-verbs)
* [Lemarios y listas de palabras del español](https://github.com/olea/lemarios)
* [SimpleNLG-ES](https://github.com/citiususc/SimpleNLG-ES)

Before running tests, you must first perform a one-time execution of [fetchExternal.sh](src/test/resources/fetchExternal.sh) to fetch the test data.

This data is available under various licenses, but is only used for testing, not at runtime, so license compatibility for redistribution of morphala should not be an issue.
