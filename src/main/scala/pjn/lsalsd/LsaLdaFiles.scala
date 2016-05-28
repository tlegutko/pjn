package pjn.lsalsd

object LsaLdaFiles {
  val resourcesPathPrefix = "src/main/resources/pjn/"
  val lsaLdaPrefix = "lsalsd/out/"
  val fullLsaLdaPrefix = resourcesPathPrefix + lsaLdaPrefix
  val termDocMatrix = resourcesPathPrefix + "vectors/out/papTermDoc"
  val lsaFile = lsaLdaPrefix + "papLSA"
  val ldaTopTopicsFile = lsaLdaPrefix + "papTopTopicsLDA"
  val sparkLDAModel = fullLsaLdaPrefix + "LDAModel"
  val topicsPerDocLDA = lsaLdaPrefix + "topicPerDocLDA"
  val topicsPerDocLSA = lsaLdaPrefix + "topicPerDocLSA"
}
