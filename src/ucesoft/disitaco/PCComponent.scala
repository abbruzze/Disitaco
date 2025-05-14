package ucesoft.disitaco

import scala.collection.mutable.ListBuffer
import javax.swing.Icon

object PCComponent:
  case class Property(name:String,value:String)
/**
 * @author Alessandro Abbruzzetti
 *         Created on 03/09/2023 20:14  
 */
trait PCComponent extends MessageBus.MessageListener:
  import PCComponent.*
  private val components = new ListBuffer[PCComponent]
  protected val componentName : String = ""
  protected var log = Logger.getLogger
  protected var componentEnabled = true
  
  protected val icon : Icon = null
  
  def getIcon: Option[Icon] = Option(icon)
  
  def getProperties: List[Property] = Nil
  
  def getComponentName: String = componentName
  
  def registerOnBus(): Unit =
    MessageBus.add(this)
    components.foreach(_.registerOnBus())
  
  final def add(c:PCComponent): Unit =
    components += c
  final def remove(c:PCComponent): Unit =
    components -= c
  
  final def isComponentEnabled: Boolean = componentEnabled
  def setComponentEnabled(enabled:Boolean): Unit =
    this.componentEnabled = enabled
  
  protected def init(): Unit = {}
  protected def reset(): Unit = {}
  protected def hardReset(): Unit = reset()
  
  final def initComponent(): Unit =
    log.info("Initializing %s",if componentName.isEmpty then getClass.getName else componentName)
    init()
    components.foreach(_.initComponent())
    
  final def resetComponent(): Unit =
    log.info("Resetting %s",componentName)
    components.foreach(_.resetComponent())
    reset()

  final def hardResetComponent(): Unit =
    log.info("Hard resetting %s",componentName)
    components.foreach(
      _.hardResetComponent())
    hardReset()

  def setLogger(logger: Logger): Unit =
    log = logger
    components.foreach(_.setLogger(logger))

  override def onMessage(msg: MessageBus.Message): Unit = {}

  final def createComponentState(): StateBuilder =
    try
      val sb = new StateBuilder()
      val csb = new StateBuilder()
      createState(csb)
      sb.w("component",csb.build())
      val compNames = components.map(_.componentName)
      val compNamesSet = compNames.toSet
      if compNames.size != compNamesSet.size || compNamesSet.contains("component") then
        throw StateBuilder.StateBuilderException(s"SMD component $componentName contains a subcomponent with 'component' name or two or more subcomponents have the same name")
      val children = new StateBuilder()
      for comp <- components do
        val state = comp.createComponentState()
        children.w(comp.componentName, state.build())
      sb.w("children",children.build())
      sb
    catch
      case se:StateBuilder.StateBuilderException =>
        se.addPath(componentName)
        throw se
      case t: Throwable =>
        val se = new StateBuilder.StateBuilderException(s"Unexpected error: $t")
        se.addPath(componentName)
        throw se  
  final def restoreComponentState(sb:StateBuilder): Unit =
    try
      sb.subStateBuilder("component") match
        case Some(comp) =>
          sb.subStateBuilder("children") match
            case Some(children) =>
              for c <- components do
                children.subStateBuilder(c.componentName) match
                  case Some(child) =>
                    c.restoreComponentState(child)
                  case None =>
                    throw new StateBuilder.StateBuilderException(s"Error while restoring ${c.componentName} child of $componentName: component not found")
              restoreState(comp)
            case None =>
              throw new StateBuilder.StateBuilderException(s"Error while restoring $componentName: cannot find 'children' attribute")
        case None =>
          throw new StateBuilder.StateBuilderException(s"Error while restoring $componentName: cannot find 'component' attribute")
    catch
      case se: StateBuilder.StateBuilderException =>
        se.addPath(componentName)
        throw se
      case t: Throwable =>
        val se = new StateBuilder.StateBuilderException(s"Unexpected error: $t",t)
        se.addPath(componentName)
        throw se

  protected def createState(sb:StateBuilder): Unit = {}
  protected def restoreState(sb:StateBuilder): Unit = {}