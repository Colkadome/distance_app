lookUpTable <- list("z"=list("label"="z", "unit_r"="", "unit_html"="", "val"="z"),
                    "a"=list("label"="a", "unit_r"="", "unit_html"="", "val"="a"),
                    "CoDist"=list("label"="Comoving Radial Distance LoS", "unit_r"="(Mpc)", "unit_html"="(Mpc)", "val"="CoDist"),
                    "LumDist"=list("label"="Luminosity Distance", "unit_r"="(Mpc)", "unit_html"="(Mpc)", "val"="LumDist"),
                    "AngDist"=list("label"="Angular Diameter Distance", "unit_r"="(Mpc)", "unit_html"="(Mpc)", "val"="AngDist"),
                    "CoDistTran"=list("label"="Comoving Radial Distance Tran", "unit_r"="(Mpc)", "unit_html"="(Mpc)", "val"="CoDistTran"),
                    "DistMod"=list("label"="Distance Modulus", "unit_r"="(mag)", "unit_html"="(mag)", "val"="DistMod"),
                    "AngSize"=list("label"="Physical Angular Size", "unit_r"="(kpc/arcsec)", "unit_html"="(kpc/arcsec)", "val"="AngSize"),
                    "CoVol"=list("label"="Comoving Volume", "unit_r"="(Gpc^{3})", "unit_html"="(Gpc<sup>3</sup>)", "val"="CoVol"),
                    "HubTime"=list("label"="Hubble Time", "unit_r"="(Gyr)", "unit_html"="(Gyr)", "val"="HubTime"),
                    "UniAgeNow"=list("label"="Universe Age Now", "unit_r"="(Gyr)", "unit_html"="(Gyr)", "val"="UniAgeNow"),
                    "UniAgeAtz"=list("label"="Universe Age", "unit_r"="(Gyr)", "unit_html"="(Gyr)", "val"="UniAgeAtz"),
                    "TravelTime"=list("label"="Look-back Time", "unit_r"="(Gyr)", "unit_html"="(Gyr)", "val"="TravelTime"),
                    "H"=list("label"="H", "unit_r"="(Km/s / Mpc)", "unit_html"="(Km/s / Mpc)", "val"="H"),
                    "OmegaM"=list("label"="OmegaM", "unit_r"="", "unit_html"="", "val"="OmegaM"),
                    "OmegaL"=list("label"="OmegaL", "unit_r"="", "unit_html"="", "val"="OmegaL"),
                    "OmegaK"=list("label"="OmegaK", "unit_r"="", "unit_html"="", "val"="OmegaK"),
                    "Factor"=list("label"="Growth Factor", "unit_r"="", "unit_html"="", "val"="Factor"),
                    "Rate"=list("label"="Growth Rate", "unit_r"="", "unit_html"="", "val"="Rate"),
                    "Sigma8"=list("label"="Sigma8", "unit_r"="", "unit_html"="", "val"="Sigma8"),
                    "RhoCrit"=list("label"="Critical Mass Density", "unit_r"="(10^{10}*Msol/Mpc^{3})", "unit_html"="(10<sup>10</sup>Msol/Mpc<sup>3</sup>)", "val"="RhoCrit")
)

defaultParams <- list(
    "737"=list(
        "H0"=70.0,
        "OmegaM"=0.30,
        "OmegaL"=0.70,
        "Sigma8"=0.80),
    "137"=list(
        "H0"=100.0,
        "OmegaM"=0.30,
        "OmegaL"=0.70,
        "Sigma8"=0.80),
    "Planck"=list(
        "H0"=67.3,
        "OmegaM"=0.315,
        "OmegaL"=0.685,
        "Sigma8"=0.829),
    "WMAP9"=list(
        "H0"=69.3,
        "OmegaM"=0.288,
        "OmegaL"=0.712,
        "Sigma8"=0.817),
    "WMAP7"=list(
        "H0"=70.4,
        "OmegaM"=0.275,
        "OmegaL"=0.725,
        "Sigma8"=0.816),
    "WMAP5"=list(
        "H0"=70.5,
        "OmegaM"=0.274,
        "OmegaL"=0.726,
        "Sigma8"=0.812),
    "WMAP3"=list(
        "H0"=70.4,
        "OmegaM"=0.268,
        "OmegaL"=0.732,
        "Sigma8"=0.776),
    "WMAP1"=list(
        "H0"=72.0,
        "OmegaM"=0.290,
        "OmegaL"=0.710,
        "Sigma8"=0.900),
    "Millennium"=list(
        "H0"=73.0,
        "OmegaM"=0.250,
        "OmegaL"=0.750,
        "Sigma8"=0.900),
    "GiggleZ"=list(
        "H0"=70.5,
        "OmegaM"=0.273,
        "OmegaL"=0.727,
        "Sigma8"=0.812),
    "Custom"=list(
        "H0"=0,
        "OmegaM"=0,
        "OmegaL"=0,
        "Sigma8"=0)
    )