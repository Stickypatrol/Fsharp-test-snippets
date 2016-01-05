type Car =
  {
    DistFromHome : int
    UpdateCar : Car -> Car
    IsCarArrived : Car -> bool
  }

type Boat =
  {
    MilesFromBermudaTriangle : int
    UpdateBoat : Boat -> Boat
    IsBoatArrived : Boat -> bool
  }
let giveCar dist = {
                      DistFromHome = dist;
                      UpdateCar = (fun car -> printfn "distance from home is %i" (car.DistFromHome - 1)
                                              {car with DistFromHome = car.DistFromHome - 1});
                      IsCarArrived = (fun car ->  if (car.DistFromHome = 0) then printfn "we have arrived at home safely!"
                                                  car.DistFromHome = 0)
                    }
let giveBoat miles = {
                        MilesFromBermudaTriangle = miles;
                        UpdateBoat = (fun (boat : Boat) ->  printfn "nautical miles from bermuda triangle is %i" (boat.MilesFromBermudaTriangle + 1)
                                                            {boat with MilesFromBermudaTriangle = boat.MilesFromBermudaTriangle + 1});
                        IsBoatArrived = (fun boat ->  if (boat.MilesFromBermudaTriangle = 10) then printfn "Oh no were being attacked by a ...!"
                                                      boat.MilesFromBermudaTriangle = 10)
                      }
let samplecar = giveCar (System.Random().Next(5, 10))
let sampleboat = giveBoat (System.Random().Next(0, 3))

type genericentity =
  | Car of Car
  | Boat of Boat

type Entity =
  {
    Update : Unit -> Entity
    IsArrived : Unit -> bool
  } with
    static member entityFromEntity (thing : genericentity) =
      match thing with
      | Car car ->  {
                      Update = fun () -> car |> car.UpdateCar |> Car |> Entity.entityFromEntity
                      IsArrived = fun () -> car |> car.IsCarArrived
                    }
      | Boat boat -> {
                      Update = fun () -> boat |> boat.UpdateBoat |> Boat |> Entity.entityFromEntity
                      IsArrived = fun () -> boat |> boat.IsBoatArrived
                    }

let entityList = [(Entity.entityFromEntity (Car(samplecar)));(Entity.entityFromEntity (Car(samplecar))); (Entity.entityFromEntity (Boat(sampleboat)))]

let rec gameLoop (gs : List<Entity>) =
  let gs' = [for (ent : Entity) in gs do if not (ent.IsArrived()) then yield ent.Update()] in
  System.Threading.Thread.Sleep(1000)
  gameLoop gs'

do gameLoop entityList