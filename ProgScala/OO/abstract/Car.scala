abstract class Vehicle {
  val move: String
}

class Car extends Vehicle {
  override val move: String = "Car is moving on the road."
}

class Truck extends Car {
  override val move: String = "Truck is loudly moving."
}

/** disable further inheritance with `final` */
final class Shuttle extends Vehicle {
  override val move: String = "Shuttle moving on the water."
}
