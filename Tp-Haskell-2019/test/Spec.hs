import PdePreludat
import Library
import Test.Hspec
import Data.List (sort)

chocolateIntenso = Helado{ 
  gusto = "chocolate intenso",
  temperatura = 0,
  ingredientes = ["chocolate", "leche", "nesquik"]
}
chocolateAguado = Helado{
  gusto = "chocolate aguado",
  temperatura = 0,
  ingredientes = ["chocolate","agua"]
}

frutillaAlAgua = Helado{
  gusto = "frutilla al agua",
  temperatura = 0,
  ingredientes = ["frutilla","agua"]  
}

frutilla = Helado{
  gusto = "frutilla",
  temperatura = 0,
  ingredientes = ["frutilla","leche"]  
}

durazno = Helado{
  gusto = "durazno",
  temperatura = -2,
  ingredientes = ["durazno","azucar","agua"]  
}

helados = [
  Helado "pistacho" 2 ["pistacho", "leche", "agua"],
  Helado "frutilla" (-2) ["frutilla","leche"],
  Helado "dulce de leche" (-1) ["dulce de leche", "agua"],
  Helado "agua" 0 ["agua"]]

heladoDe gusto grados = gusto{ temperatura =  grados  }

main :: IO ()
main = hspec $ do

  describe"Punto 1:" $do
    describe "Helados bien y mal preparados" $ do

      it "El helado de chocolate intenso a -20 grados, esta bien preparado" $ do
        (heladoDe chocolateIntenso (-20) ) `shouldSatisfy` estaBienPreparado 
      
      it "El helado de chocolate intenso a -10 grados, esta bien preparado" $ do
         (heladoDe chocolateIntenso (-10)) `shouldSatisfy` estaBienPreparado
    
      it "EL helado de chocolate intenso a -2 grados, NO esta bien preparado " $ do
         (heladoDe chocolateIntenso (-2)) `shouldNotSatisfy` estaBienPreparado
    
--      it "EL helado de chocolate aguado a -6 grados con chocolate y agua NO esta bien preparado" $ do
--         (heladoDe chocolateAguado 6)`shouldNotSatisfy` estaBienPreparado

      it "EL helado de frutilla al agua a -6 grados con frutilla y agua, esta bien preparado" $ do
         (heladoDe frutillaAlAgua (-6))`shouldSatisfy` estaBienPreparado
    
      it "EL helado de frutilla al agua a -5 grados  con frutilla y agua, esta bien preparado" $ do
         (heladoDe frutillaAlAgua (-5))  `shouldSatisfy` estaBienPreparado

      it "EL Helado de frutilla al agua a -3 grados con frutilla y agua, NO esta bien preparado" $ do
        (heladoDe frutillaAlAgua (-3))  `shouldNotSatisfy` estaBienPreparado

      it "El helado de frutilla a -3 grados con frutilla y leche, esta bien preparado" $ do
         (heladoDe frutilla (-3))  `shouldSatisfy` estaBienPreparado

      it "El Helado de frutilla a -2 grados  con  frutilla y leche, esta bien preparado" $ do
         (heladoDe frutilla (-2))  `shouldSatisfy` estaBienPreparado

      it "El Helado de frutilla a -1 grados con  frutilla y leche, NO esta bien preparado" $ do
         (heladoDe frutilla (-1))  `shouldNotSatisfy` estaBienPreparado

  describe "Punto 2:" $do
    describe "Modelando la heladera" $ do

      it "Enfriar con la heladera 5 grados el helado de frutilla al agua de -6 grados, la temperatura debe dar -11 grados" $do
        (temperatura.heladera (-5)) (heladoDe frutillaAlAgua (-6)) `shouldBe` -11 
      
      it "Enfriar con la heladara 8 grados el helado de chocolate intenso de -2 grados, para que este bien preparado" $do
        (heladoDe chocolateIntenso (-2))  `shouldSatisfy`  estaBienPreparado.heladera (-8)
    
    describe "Modelando la agregadora de ingredientes" $do  
      
      it "Agregar crema al helado de chocolate bien preparado de -22 grados, para que tenga 4 ingredientes" $ do
        (cantidadDeIngrediente.agregadora"crema") (heladoDe chocolateIntenso 22) `shouldBe` 4
      
      it "Agregar crema al helado de chocolate de -22 grados, por lo tanto el ultimo ingrediente debe ser la crema" $ do
        (last.ingredientes.agregadora "crema") (heladoDe chocolateIntenso (-22)) `shouldBe` "crema"
    
    describe"Modelando la mixturadora" $ do

      it "Al mezclar el helado de frutilla al agua de -6 grados y el de durazno de -2 grados, por lo tanto el gusto debe ser frutilla al agua y durazno" $ do
        (gusto.mixturadora (heladoDe frutillaAlAgua (-6))) durazno `shouldBe` "frutilla al agua y durazno"
      
      it "Al mezclar el helado de frutilla al agua de -6 grados y el de durazno de -2 grados, la temperatura debe ser -6 grados" $do 
        (temperatura.mixturadora (heladoDe frutillaAlAgua (-6))) durazno `shouldBe` -6
      
      it "Al mezclar el helado de frutilla al agua de -6 grados y el de durazno de -2 grados, los ingredientes deben ser agua, azucar, durazno y frutilla" $do
        (sort.ingredientes.mixturadora (heladoDe frutillaAlAgua (-6))) durazno `shouldBe` ["agua","azucar","durazno","frutilla"]
   
    describe "Modelando el batidor" $ do
    
      it "Al batir un cajon de banana con un dispenser de -4 grados y un helado neutro, el gusto debe ser banana" $ do
        (gusto.batidor "banana"  (-4))  heladoNeutro `shouldBe` "banana"
      it "Al batir un cajon de banana con un dispenser de -4 grados y un helado neutro, la temperatura debe estar a -4 grados" $do  
        (temperatura.batidor "banana"  (-4))  heladoNeutro `shouldBe` -4   
      
      it "Al batir un cajon de banana con un dispenser de -4 grados y un helado neutro, los ingredientes deben ser agua y banana" $ do
        (sort.ingredientes.batidor "banana"  (-4))  heladoNeutro `shouldBe` ["agua","banana"]     
        
    
    describe "Modelando la choripasteadora" $do
    
      it"choripasteando el helado de chocolate intenso bien preparado de -22 grados, el gusto debe ser chocolate intenso de la casa" $do
        (gusto.choripastear) (heladoDe chocolateIntenso (-22)) `shouldBe` "chocolate intenso de la casa"
      
      it"choripasteando el helado de chocolate intenso bien preparado de -22 grados, la temperatura debe ser -22 grados" $do
        (temperatura.choripastear) (heladoDe chocolateIntenso (-22)) `shouldBe` -22 
      
      it"choripasteando el helado de chocolate intenso bien preparado de -22 grados, los agua, chocolate, esencia artificial, leche y nesquik " $do
        (sort.ingredientes.choripastear) (heladoDe chocolateIntenso (-22)) `shouldBe` ["agua", "chocolate", "esencia artificial", "leche", "nesquik"]

    describe "Helado Neutro" $ do
     
      it "Al aplicar al helado neutro la batidora  de frutilla, el gusto debe ser frutilla " $ do
       (gusto.agregadora "azucar" .heladera (-5).batidor "frutilla" (-5)) heladoNeutro `shouldBe` "frutilla"
      
      it "Al aplicar al helado neutro,la batidora de frutilla con un dispenser de -5 y enfriarlo 5 grados con la heladera, la temperatura debe ser -10 grados" $ do
       (temperatura.agregadora "azucar" .heladera (-5).batidor "frutilla" (-5)) heladoNeutro `shouldBe` -10
      
      it "Al aplicar al helado neutro la agregadora con azucar, los ingredientes deben ser agua, frutilla y azucar" $ do
       (ingredientes.agregadora "azucar" .heladera (-5).batidor "frutilla" (-5)) heladoNeutro `shouldBe` ["agua","frutilla","azucar"]

{-
        La aplicacion parcial aparece en :
        
        agregadora "azucar",
        heladera (-5),
        batidor "frutilla" (-5)

        Como se ve se esta utilizando a la funciones con menos parametros de lo necesario , haciendo que se 
        aplique parcialmente los parametros y asi nos devuelva una nueva funcion.
        Dado a esto podemos componerlas tambien.
-}

  describe "Punto 3:" $ do
      describe"Chocolate infinito" $do 
        
        it "Al enfriar con la heladera -2 grados el helado de chocolate infinito , la temperatura debe ser -6 grados" $ do
            (temperatura.heladera (-2)) chocolateInfinito`shouldBe` -6
{-
    Es posible usar la heladera y obtener la temperatura nueva del helado de chocolate
    por la evalucion perezosa de haskell (Lazy Evaluation) dado que esta evalucion toma lo necesario
    para poder usar las funciones sin importarle lo demas que tenga

¿Es posible combinar el chocolate infinito con un helado de frutilla? Justificar.
    No es posible dado que chocolate infinito posee una lista potencialmente infinita y
    no la puede evaluar como esta dada la funcion que combina los dos helados (mixturadora).
-}
  describe "Punto 4:" $ do
      describe"Cinta trasportadora:" $ do 
      
          it "Al aplicar al helado neutro la batidora de frutilla con el dispenser a -5 y enfriarlo 5 grados con la heladera, la temperatura debe ser -10 grados" $ do
            (temperatura.cintaTransportadora) [(batidor "frutilla" (-5)), (heladera (-5)), (agregadora "azucar")]   `shouldBe` -10
          
          it "Al aplicar al helado neutro la agregadora con el ingrediente azucar, los ingredientes deben ser agua, frutilla y azucar" $ do
            (ingredientes.cintaTransportadora) [(batidor "frutilla" (-5)), (heladera (-5)), (agregadora "azucar")]  `shouldBe`  ["agua","frutilla","azucar"]
      
      describe "Maquinas que preparan bien un helado" $ do  
        
        it "4 maquinas preparan bien el helado de frutilla al agua de -5 grados con ingredientes frutilla y agua" $ do
         (length.maquinasQuePreparanBien (heladoDe frutillaAlAgua (-5))) [(agregadora "frutilla"), (heladera (-5)), (heladera 10), (batidor "anana" (-25)),(mixturadora heladoNeutro)] `shouldBe` 4
        
        it "1 maquina preparan bien el helado de chocolate intenso de -2 grados" $ do
          (length.maquinasQuePreparanBien (heladoDe chocolateIntenso (-2))) [(agregadora "frutilla"), (heladera (-5)), (heladera 10), (batidor "anana" (-25)),(mixturadora heladoNeutro)] `shouldBe` 1

  describe "Bonus: " $ do    
    describe "Ingrediente favorito" $do      
       
        it "De la lista de helados, el ingredente mas usado debe ser el agua" $ do
            ingredienteFavorito helados`shouldBe` "agua"
        
