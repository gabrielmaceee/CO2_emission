using MLJ
using JLD2
using DataFrames, PlotlyJS, CSV, RDatasets
using Mon_Package_Julia
SVC = MLJ.@load SVC pkg=LIBSVM
KNNClassifier = MLJ.@load KNNClassifier
MyDecisionTree = MLJ.@load DecisionTreeClassifier pkg = DecisionTree
MyDecisionTree2 = MLJ.@load DecisionTreeRegressor pkg = DecisionTree



df = CSV.read(string(@__FILE__, "/../../Data/CO2 Emissions_Canada.csv"), DataFrames.DataFrame)
rename!(df, :"CO2 Emissions(g/km)"=> "CO2")
rename!(df, :"Fuel Consumption Comb (L/100 km)"=>"Comb")
rename!(df, :"Fuel Type"=>"Fuel")
p1 = Plot(df, x=:Comb, y=:CO2, mode="markers", marker_size=8, text=:Make, group=:Fuel)

using Dash

app = dash()

app.layout = html_div([
    html_h4("CO2 = f(consommation mixte)"),
    dcc_graph(
        id = "example-graph-3",
        figure = p1,
    ),
    dcc_input(id = "Make", value = "Marque", type = "text"),
    dcc_input(id = "Model", value = "Modèle", type = "text"),
    dcc_input(id = "Class", value = "Classe", type = "text"),
    dcc_input(id = "Taille", value = "Taille du moteur", type = "text"),
    dcc_input(id = "Cylinders", value = "Nombre de cylindres", type = "text"),
    dcc_input(id = "Transmission", value = "Transmission", type = "text"),
    dcc_input(id = "Fuel", value = "Carburant", type = "text"),
    dcc_input(id = "City", value = "Consommation en ville", type = "text"),
    dcc_input(id = "Hwy", value = "Consommmation sur autoroute", type = "text"),
    dcc_input(id = "Comb", value = "Consommation mixte", type = "text"),
    html_tr((html_td("Prediction par knn : "), html_td(id = "pred_knn"))),
    html_tr((html_td("Prediction par svm : "), html_td(id = "pred_svm"))),
    html_tr((html_td("Prediction par arbre de classification : "), html_td(id = "pred_arbre_int"))),
    html_tr((html_td("Prediction par arbre de régression : "), html_td(id = "pred_arbre_val")))
    ])

callback!(app, Output("pred_knn", "children"), Output("pred_svm", "children"), Output("pred_arbre_int", "children"), Output("pred_arbre_val", "children"), 
    Input("Make", "value"),
    Input("Model", "value"),
    Input("Class", "value"),
    Input("Taille", "value"),
    Input("Cylinders", "value"),
    Input("Transmission", "value"),
    Input("Fuel", "value"),
    Input("City", "value"),
    Input("Hwy", "value"),
    Input("Comb", "value"),
) do Make, Model, Class, Taille, Cylinders, Transmission, Fuel, City, Hwy, Comb
if Taille == "Taille du moteur" || Cylinders == "Nombre de cylindres" || City == "Consommation en ville" || Hwy == "Consommation sur autoroute" || Comb == "Consommation mixte" 
    return ("Non","Non","Non","Non")
end
    if Taille == "" || Taille == nothing || Cylinders == "" || Cylinders == nothing || City == "" || City == nothing || Hwy == "" || Hwy == nothing || Comb == "" || Comb == nothing
        return ("Non","Non","Non","Non")
    end
    if Make == "" || Make == nothing || Model == "" || Model == nothing || Class == "" || Class == nothing || Transmission == "" || Transmission == nothing || Fuel == "" || Fuel == nothing
        return (
            Mon_Package_Julia.predire_knn(Make, Model, Class, parse(Float64, Taille), parse(Float64, Cylinders), Transmission, Fuel, parse(Float64, City), parse(Float64,Hwy), parse(Float64, Comb)),
            Mon_Package_Julia.predire_svm(Make, Model, Class, parse(Float64, Taille), parse(Float64, Cylinders), Transmission, Fuel, parse(Float64, City), parse(Float64,Hwy), parse(Float64, Comb)),
        "Non","Non")
    end
    p = Mon_Package_Julia.predire_val_arbre(Make, Model, Class, parse(Float64, Taille), parse(Float64, Cylinders), Transmission, Fuel, parse(Float64, City), parse(Float64,Hwy), parse(Float64, Comb))[1]
    #scatter!([parse(Int64,Comb)],[trunc(Int32,p)], color="black", label = "nouveau", marker_size= 12)
    s = string.(p)
    return (
    Mon_Package_Julia.predire_knn(Make, Model, Class, parse(Float64, Taille), parse(Float64, Cylinders), Transmission, Fuel, parse(Float64, City), parse(Float64,Hwy), parse(Float64, Comb)),
    Mon_Package_Julia.predire_svm(Make, Model, Class, parse(Float64, Taille), parse(Float64, Cylinders), Transmission, Fuel, parse(Float64, City), parse(Float64,Hwy), parse(Float64, Comb)),
    Mon_Package_Julia.predire_arbre_int(Make, Model, Class, parse(Float64, Taille), parse(Float64, Cylinders), Transmission, Fuel, parse(Float64, City), parse(Float64,Hwy), parse(Float64, Comb)),
    "La prédiction est " * s * " g/100km")
end

run_server(app, "0.0.0.0", debug=true)
#http://localhost:8050/
