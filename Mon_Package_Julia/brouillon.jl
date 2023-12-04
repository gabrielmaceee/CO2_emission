
println(string(Mon_Package_Julia.predire_knn("PEUGEOT","206+","COMPACT",1.1,4,"jsp","X",9,6,8)))

println(string(Mon_Package_Julia.predire_arbre_int("PEUGEOT","206+","COMPACT",1.1,4,"jsp","X",9,6,8)))
println(string(Mon_Package_Julia.predire_svm("PEUGEOT","206+","COMPACT",1.1,4,"jsp","X",9,6,8)))
println(string(Mon_Package_Julia.predire_val_arbre("PEUGEOT","206+","COMPACT",1.1,4,"jsp","X",9,6,8)))

function f(x)
    if x == "E" return 0
    end
    return 1
end
export f
include("functions.jl")
end
""""
using MLJ
using JLD2
using DataFrames

function f(x)
    if x == "E" return 0
    end
    return 1
end
using JLD2
#save_object("D:/Master/S7/R-Julia/Projet/f.jld2", f)

#f=load_object("f.jld2")

function predire_svm(Marque, Modele, Class, Taille_moteur, Cylindres, Transmission,Fuel ,City,Hwy,Comb)
    c_r = load_object("centrer_reduire.jld2")
    SVC = MLJ.@load SVC pkg=LIBSVM
    svc= load_object("D:/Master/S7/R-Julia/Projet/svm_classif.jld2")
    fuel =  f(Fuel)  
    donnees = DataFrame(Engine=Taille_moteur, Cylinders = Cylindres,City=City, Hwy=Hwy, Comb=Comb )
    donnees = MLJ.transform(c_r, donnees)
    x1 = DataFrame(fuel=fuel)
    donnees=hcat(x1, donnees)
    #s = MLJ.predict(svc, donnees)
    s = string.(MLJ.predict(svc, donnees))
    s = s[1]
    return "L'intevalle svc est " * s[4:length(s)-1] * "] g/km"
        
end

println(predire_svm("PEUGEOT","206+","COMPACT",1.1,4,"jsp","X",9,6,8))

function predire_knn(Marque, Modele, Class, Taille_moteur, Cylindres, Transmission,Fuel ,City,Hwy,Comb)
    c_r = load_object("D:/Master/S7/R-Julia/Projet/centrer_reduire.jld2")
    KNNClassifier = MLJ.@load KNNClassifier
    knn = JLD2.load_object("D:/Master/S7/R-Julia/Projet/knn_classif.jld2")
    fuel =  f(Fuel)  
    donnees = DataFrame(Engine=Taille_moteur, Cylinders = Cylindres,City=City, Hwy=Hwy, Comb=Comb )
    donnees = MLJ.transform(c_r, donnees)
    x1 = DataFrame(fuel=fuel)
    donnees=hcat(x1, donnees)
    s = string.(predict_mode(knn, donnees))
    s = s[1]
    return "L'intevalle knn est " * s[4:length(s)-1] * "] g/km"
    
end

println(predire_knn("PEUGEOT","206+","COMPACT",1.1,4,"jsp","X",9,6,8))

function predire_arbre_int(Marque, Modele, Class, Taille_moteur, Cylindres, Transmission,Fuel ,City,Hwy,Comb)
    mpg = Comb *235.214583
    MyDecisionTree = MLJ.@load DecisionTreeClassifier pkg = DecisionTree
    arbre_class = load_object("D:/Master/S7/R-Julia/Projet/arbre_classif.jld2")
    donnees = DataFrame(Make=Marque, Model=Modele, Class=Class, Engine=Taille_moteur, Cylinders=Cylindres, Transmission=Transmission,Fuel=Fuel,City=City, Hwy=Hwy, Comb=Comb, mpg=mpg )
    s = string.(MLJ.predict_mode(arbre_class, donnees))
    s = s[1]
        return "L'intevalle arbre est " * s[4:length(s)-1] * "] g/km"
    
end

println(predire_arbre_int("PEUGEOT","206+","COMPACT",1.1,4,"jsp","X",9,6,8))

function predire_val_arbre(Marque, Modele, Class, Taille_moteur, Cylindres, Transmission,Fuel ,City,Hwy,Comb)
    mpg = Comb *235.214583
    MyDecisionTree2 = MLJ.@load DecisionTreeRegressor pkg = DecisionTree
    arbre_predict = load_object("D:/Master/S7/R-Julia/Projet/arbre_predict.jdl2")
    fuel =  f(Fuel)  
    donnees = DataFrame(Make=Marque, Model=Modele, Class=Class, Engine=Taille_moteur, Cylinders=Cylindres, Transmission=Transmission,Fuel=Fuel,City=City, Hwy=Hwy, Comb=Comb, mpg=mpg )
    x1 = DataFrame(fuel=fuel)
    donnees=hcat(x1, donnees)
    #s = string.(MLJ.predict_mode(arbre_predict, donnees))
    #s = s[1]
    #return "La prédiction arbre est " * s
    return MLJ.predict_mode(arbre_predict, donnees)[1]
end

println(predire_val_arbre("PEUGEOT","206+","COMPACT",1.1,4,"jsp","X",9,6,8))
"""