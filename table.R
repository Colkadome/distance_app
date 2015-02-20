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
                    "RhoCrit"=list("label"="Critical Mass Density", "unit_r"="(10^{10}*Msol/Mpc^{3})", "unit_html"="(10<sup>10</sup>Msol/Mpc<sup>3</sup>)", "val"="RhoCrit")
)

defaultParams <- list(
    "737"=list(
        "H0"=70.0,
        "OmegaM"=0.30,
        "OmegaL"=0.70),
    "Planck"=list(
        "H0"=67.11,
        "OmegaM"=0.3175,
        "OmegaL"=0.6825),
    "Planck + lens"=list(
        "H0"=68.14,
        "OmegaM"=0.3036,
        "OmegaL"=0.6964),
    "Planck + WP"=list(
        "H0"=67.04,
        "OmegaM"=0.3183,
        "OmegaL"=0.6817),
    "WMAP9"=list(
        "H0"=69.7,
        "OmegaM"=0.2821,
        "OmegaL"=0.7181),
    "WMAP7"=list(
        "H0"=70.4,
        "OmegaM"=0.2715,
        "OmegaL"=0.728),
    "WMAP5"=list(
        "H0"=70.2,
        "OmegaM"=0.2769,
        "OmegaL"=0.723),
    "WMAP3"=list(
        "H0"=70.4,
        "OmegaM"=0.268,
        "OmegaL"=0.732),
    "WMAP1"=list(
        "H0"=72.0,
        "OmegaM"=0.290,
        "OmegaL"=0.710),
    "Millennium"=list(
        "H0"=73.0,
        "OmegaM"=0.250,
        "OmegaL"=0.750),
    "GiggleZ"=list(
        "H0"=70.5,
        "OmegaM"=0.2736,
        "OmegaL"=0.726),
    "Custom"=list(
        "H0"=0,
        "OmegaM"=0,
        "OmegaL"=0)
    )