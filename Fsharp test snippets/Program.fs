type Direction =
  | X
  | Y


type Car =
  {
    DistFromHome : int
    UpdateCar : Car -> Car
    IsCarArrived : Car -> bool
    getInfo : Car -> Direction
  }

type Boat =
  {
    MilesFromBermudaTriangle : int
    UpdateBoat : Boat -> Boat
    IsBoatArrived : Boat -> bool
  }
let giveCar() = {
                      DistFromHome = System.Random().Next(5, 10);
                      UpdateCar = (fun car -> printfn "distance from home is %i" (car.DistFromHome - 1)
                                              {car with DistFromHome = car.DistFromHome - 1});
                      IsCarArrived = (fun car ->  if (car.DistFromHome = 0) then printfn "we have arrived at home safely!"
                                                  car.DistFromHome = 0)
                    }
let giveBoat() = {
                        MilesFromBermudaTriangle = System.Random().Next(0, 3);
                        UpdateBoat = (fun (boat : Boat) ->  printfn "nautical miles from bermuda triangle is %i" (boat.MilesFromBermudaTriangle + 1)
                                                            {boat with MilesFromBermudaTriangle = boat.MilesFromBermudaTriangle + 1});
                        IsBoatArrived = (fun boat ->  if (boat.MilesFromBermudaTriangle = 10) then printfn "Oh no were being attacked by a ...!"
                                                      boat.MilesFromBermudaTriangle = 10)
                      }

  
type GenericEntity =
  | Car of Car
  | Boat of Boat

let rec collide (thing:GenericEntity) (things:List<GenericEntity>) : Option<Direction> =
  let collides thinga thingb =
    Some(X)
  match things with
  | h::t -> if collides h thing = None then
              collide thing t
            else
              collides h thing
  | [] -> None

type Entity =
  {
    Update : Unit -> Entity
    IsArrived : Unit -> bool
    GetInto : List<GenericEntity> -> Direction
  } with
    static member entityFromEntity (thing : GenericEntity) =
      match thing with
      | Car car ->  {
                      Update = fun () -> car |> car.UpdateCar |> Car |> Entity.entityFromEntity
                      IsArrived = fun () -> car |> car.IsCarArrived

                    }
      | Boat boat -> {
                      Update = fun () -> boat |> boat.UpdateBoat |> Boat |> Entity.entityFromEntity
                      IsArrived = fun () -> boat |> boat.IsBoatArrived
                    }
    static member getinfo (thing : GenericEntity) =
      (*match thing with the types of entity then call the getinfo method of the object you want*)
      
let entityList = [(Entity.entityFromEntity (Car(giveCar())));(Entity.entityFromEntity (Car(giveCar()))); (Entity.entityFromEntity (Boat(giveBoat())))]

let rec gameLoop (gs : List<Entity>) =
  let gs' = [for (ent : Entity) in gs do if not (collide ent. gs) then yield ent.Update()] in
  System.Threading.Thread.Sleep(1000)
  gameLoop gs'

do gameLoop entityList