module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2

  describe "Test de Missisipi Queen" $ do
    it "Morde puede hacer la mision Missisipi Queen" $ do
      puedeHacerMision beberMissisipiQueen mordecai `shouldBe` True 
    it "Rigby no puede hacer la mision Missisipi Queen por vago" $ do
      puedeHacerMision beberMissisipiQueen rigby `shouldBe` False --Rigby tiene vagancia 101
    it "Benson puede hacer la mision Missisipi Queen" $ do
      puedeHacerMision beberMissisipiQueen benson `shouldBe` True
    it "Rigby ahora si puede hacer la mision Missisipi Queen" $ do
      puedeHacerMision beberMissisipiQueen rigbyNoVago `shouldBe` True
    it "Morde no puede hacer la mision Missisipi Queen por vago" $ do
      puedeHacerMision beberMissisipiQueen mordecaiVago `shouldBe` False
    it "Musculoso no puede hacer la mision Missisipi Queen" $ do
      puedeHacerMision beberMissisipiQueen musculoso `shouldBe` False

benson :: Personaje
benson = UnPersonaje {nombrePersonaje = "Benson", nivelInteligencia = 125, nivelVagancia = 25, mejoras = [picante, fiestaLosMartes], titulos = ["El Baterista"]}

rigbyNoVago :: Personaje
rigbyNoVago = rigby {nivelVagancia = 40}

mordecaiVago :: Personaje
mordecaiVago = mordecai {nivelVagancia = 85}

musculoso :: Personaje
musculoso = UnPersonaje {nombrePersonaje = "Musculoso", nivelInteligencia = 10, nivelVagancia = 30, mejoras = [fiestaLosMartes], titulos = ["Mi Mami"]}
