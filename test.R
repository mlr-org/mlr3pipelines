load_all("~/cos/phng")
load_all("~/cos/mlrng")
load_all()

task = mlr.tasks$get("iris")


cp = cpoScale %>>% cpoPca %>>% cpoNull

z = trainPipe(task, cp) 
w = predictPipe(task, z)


