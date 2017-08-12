java -Xmx1024M -Xss2M -XX:+CMSClassUnloadingEnabled -XX:MaxMetaspaceSize=512M -jar `dirname $0`/sbt-launch-0.13.8.jar "$@"
