# SysSon Site Generation

__To build the docs locally, run `sbt unidoc`. You can view the results via `open target/scala-2.10/unidoc/index.html`.__

Alternatively, you can run the site via a local web server as `sbt preview-site` which is a functionality of the [sbt-site](https://github.com/sbt/sbt-site) plugin. I publish the
 results to GitHub using `sbt ghpages-push-site` which is provided by the [sbt-ghpages](https://github.com/sbt/sbt-ghpages) plugin.
