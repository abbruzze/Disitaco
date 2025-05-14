package ucesoft.disitaco

/**
 * @author Alessandro Abbruzzetti
 *         Created on 30/03/2025 18:17  
 */
object Config:
  private var homeDir = System.getProperty("user.home")
  
  def setHomeDire(home:String): Unit = 
    homeDir = home
    
  def getHomeResource(homeRelativePath:String): Option[Array[Byte]] =
    val path = java.nio.file.Paths.get(homeDir,homeRelativePath)
    if java.nio.file.Files.exists(path) then
      Some(java.nio.file.Files.readAllBytes(path))
    else
      None
      
  def getHomeResourceAsString(homeRelativePath:String): Option[String] = getHomeResource(homeRelativePath).map(new String(_,"UTF-8"))
  
  def getResource(path:String): Option[Array[Byte]] =
    val in = getClass.getResourceAsStream(path)
    if in != null then
      Some(in.readAllBytes())
    else
      None
  def getResourceAsString(path:String): Option[String] = getResource(path).map(new String(_,"UTF-8"))
