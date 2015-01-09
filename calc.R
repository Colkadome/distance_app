# contains calculations for the distance app

cosdist=function (z = 0, H0 = 100, OmegaM = 0.3, OmegaL = 0.7, OmegaK = 0, 
                  age = FALSE) 
{
  temp = function(z, H0, OmegaM, OmegaL, OmegaK) {
    Einv = function(z, OmegaM, OmegaL, OmegaK) {
      1/sqrt(OmegaM * (1 + z)^3 + OmegaK * (1 + z)^2 + 
               OmegaL)
    }
    CoDist = (299792.458/H0) * integrate(Einv, 0, z, OmegaM = OmegaM, 
                                         OmegaL = OmegaL, OmegaK = OmegaK, subdivisions = 1000)$value
    CoVol = (4/3) * pi * CoDist^3
    LumDist = (1 + z) * CoDist
    AngDist = CoDist/(1 + z)
    LumVol = CoVol * (1 + z)^3
    AngVol = CoVol / (1 + z)^3
    AngArcSec = AngDist * (pi/(180 * 60 * 60))
    if (age) {
      Einvz = function(z, OmegaM = 0.27, OmegaL = 0.73, 
                       OmegaK = 0) {
        1/(sqrt(OmegaM * (1 + z)^3 + OmegaK * (1 + z)^2 + 
                  OmegaL) * (1 + z))
      }
      HT = 3.08568025e+19/(H0 * 31556926)
      UniAge = HT * integrate(Einvz, 0, Inf, OmegaM = OmegaM, 
                              OmegaL = OmegaL, OmegaK = OmegaK, subdivisions = 1000)$value
      zAge = HT * integrate(Einvz, 0, z, OmegaM = OmegaM, 
                            OmegaL = OmegaL, OmegaK = OmegaK, subdivisions = 1000)$value
    }
    if (age) {
      return = c(z = z, CoDist = CoDist, LumDist = LumDist, 
                 AngDist = AngDist, AngArcSec = AngArcSec, CoVolGpc3 = CoVol/1e+09, 
                 HubTime = HT, UniAge = UniAge, TravelTime = zAge, LumVolGpc3 = LumVol/1e+09, AngVolGpc3 = AngVol/1e+09)
    }
    else {
      return = c(z = z, CoDist = CoDist, LumDist = LumDist, 
                 AngDist = AngDist, AngArcSec = AngArcSec, CoVolGpc3 = CoVol/1e+09, LumVolGpc3 = LumVol/1e+09, AngVolGpc3 = AngVol/1e+09)
    }
  }
  return = as.data.frame(t(Vectorize(temp)(z = z, H0 = H0, OmegaM = OmegaM, 
                             OmegaL = OmegaL, OmegaK = OmegaK)))
}
