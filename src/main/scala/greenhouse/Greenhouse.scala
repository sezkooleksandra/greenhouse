package greenhouse

sealed trait Event
final case class Refill(liters: Double) extends Event
final case class Irrigate(sessionId: String, liters: Double, durationMin: Double) extends Event
final case class AddFertilizer(concentrateLiters: Double) extends Event
final case class DrainSludge(liters: Double) extends Event

sealed trait DomainError
final case class InvalidAmount(value: Double) extends DomainError
final case class TankOverflow(requested: Double, freeSpace: Double) extends DomainError
final case class BelowMinSensor(level: Double, minLevel: Double) extends DomainError
final case class InsufficientWater(requested: Double, available: Double) extends DomainError
final case class DuplicateSession(id: String) extends DomainError

final case class TankConfig(capacity: Double, minLevel: Double) {
  require(capacity > 0, "capacity must be positive")
  require(minLevel >= 0 && minLevel < capacity, "minLevel must be in [0, capacity)")
}

final case class IrrigationSession(id: String, liters: Double, durationMin: Double) {
  def intensity: Double = liters / durationMin
}

final case class TankState(
    waterLevel: Double = 0.0,
    fertilizerLiters: Double = 0.0,
    sessions: Vector[IrrigationSession] = Vector.empty
)

object Greenhouse {

  val initial: TankState = TankState()

  private def positive(v: Double): Either[DomainError, Double] =
    if (v.isNaN || v.isInfinite || v <= 0) Left(InvalidAmount(v)) else Right(v)

  def apply(cfg: TankConfig)(state: TankState, event: Event): Either[DomainError, TankState] =
    event match {

      case Refill(l) =>
        for {
          _ <- positive(l)
          _ <- Either.cond(
            state.waterLevel + l <= cfg.capacity,
            (),
            TankOverflow(l, cfg.capacity - state.waterLevel)
          )
        } yield state.copy(waterLevel = state.waterLevel + l)

      case Irrigate(id, l, dur) =>
        for {
          _ <- positive(l)
          _ <- positive(dur)
          _ <- Either.cond(!state.sessions.exists(_.id == id), (), DuplicateSession(id))
          _ <- Either.cond(
            state.waterLevel >= cfg.minLevel,
            (),
            BelowMinSensor(state.waterLevel, cfg.minLevel)
          )
          _ <- Either.cond(
            l <= state.waterLevel,
            (),
            InsufficientWater(l, state.waterLevel)
          )
        } yield state.copy(
          waterLevel = state.waterLevel - l,
          sessions = state.sessions :+ IrrigationSession(id, l, dur)
        )

      case AddFertilizer(c) =>
        positive(c).map(_ => state.copy(fertilizerLiters = state.fertilizerLiters + c))

      case DrainSludge(l) =>
        for {
          _ <- positive(l)
          _ <- Either.cond(l <= state.waterLevel, (), InsufficientWater(l, state.waterLevel))
        } yield state.copy(waterLevel = state.waterLevel - l)
    }

  def process(cfg: TankConfig, events: Seq[Event]): Either[DomainError, TankState] =
    events.foldLeft[Either[DomainError, TankState]](Right(initial)) {
      case (acc, e) => acc.flatMap(apply(cfg)(_, e))
    }


  def currentLevel(state: TankState): Double = state.waterLevel

  def totalIrrigated(state: TankState): Double = state.sessions.map(_.liters).sum

  def topIntensive(state: TankState, n: Int = 2): List[IrrigationSession] =
    state.sessions.sortBy(s => -s.intensity).take(n).toList
}
