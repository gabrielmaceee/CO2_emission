using MLJ
using JLD2
using DataFrames, PlotlyJS, CSV, RDatasets
function f(x)
    if x == "E" return 0
    end
    return 1
end

function predire_knn(Marque, Modele, Class, Taille_moteur, Cylindres, Transmission,Fuel ,City,Hwy,Comb)   
    c_r = load_object(string(@__FILE__, "/../" , "centrer_reduire.jld2"))
    knn = JLD2.load_object(string(@__FILE__, "/../" , "knn_classif.jld2" ))
    fuel =  f(Fuel)  
    donnees = DataFrame(Engine=Taille_moteur, Cylinders = Cylindres,City=City, Hwy=Hwy, Comb=Comb )
    donnees = MLJ.transform(c_r, donnees)
    x1 = DataFrame(fuel=fuel)
    donnees=hcat(x1, donnees)
    s = string.(predict_mode(knn, donnees))
    s = s[1]
    return "L'intevalle est " * s[4:length(s)-1] * "] g/100km"
    
end

function predire_svm(Marque, Modele, Class, Taille_moteur, Cylindres, Transmission,Fuel ,City,Hwy,Comb)
    c_r = load_object(string(@__FILE__, "/../" , "centrer_reduire.jld2"))
    svc= load_object(string(@__FILE__, "/../" , "svm_classif.jld2"))
    fuel =  f(Fuel)  
    donnees = DataFrame(Engine=Taille_moteur, Cylinders = Cylindres,City=City, Hwy=Hwy, Comb=Comb )
    donnees = MLJ.transform(c_r, donnees)
    x1 = DataFrame(fuel=fuel)
    donnees=hcat(x1, donnees)
    s = MLJ.predict(svc, donnees)
    s = string.(MLJ.predict(svc, donnees))
    s = s[1]
    return "L'intevalle est " * s[4:length(s)-1] * "] g/100km" 
end


function predire_arbre_int(Marque, Modele, Class, Taille_moteur, Cylindres, Transmission,Fuel ,City,Hwy,Comb)
    mpg = Comb *235.214583
    arbre_class = load_object(string(@__FILE__, "/../" , "arbre_classif.jld2"))
    donnees = DataFrame(Make=Marque, Model=Modele, Class=Class, Engine=Taille_moteur, Cylinders=Cylindres, Transmission=Transmission,Fuel=Fuel,City=City, Hwy=Hwy, Comb=Comb, mpg=mpg )
    s = string.(MLJ.predict_mode(arbre_class, donnees))
    s = s[1]
        return "L'intevalle est " * s[4:length(s)-1] * "] g/100km"
    
end

function predire_val_arbre(Marque, Modele, Class, Taille_moteur, Cylindres, Transmission,Fuel ,City,Hwy,Comb)
    mpg = Comb *235.214583
    arbre_predict = load_object(string(@__FILE__, "/../" , "arbre_predict.jdl2"))
    fuel =  f(Fuel)  
    donnees = DataFrame(Make=Marque, Model=Modele, Class=Class, Engine=Taille_moteur, Cylinders=Cylindres, Transmission=Transmission,Fuel=Fuel,City=City, Hwy=Hwy, Comb=Comb, mpg=mpg )
    x1 = DataFrame(fuel=fuel)
    donnees=hcat(x1, donnees)
    return MLJ.predict_mode(arbre_predict, donnees)[1]   
end

export f, predire_knn, predire_svm, predire_arbre_int, predire_val_arbre
export knn
