SELECT l.objID, l.ra as ra_sdssdr7, l.dec as dec_sdssdr7, r.RA as ra_rosatsdssdr5, r.DE as dec_rosatsdssdr5, n.distance, l.objID, l.plate, l.mjd, l.fiberID, l.F_Halpha, l.EW_Halpha, l.s2n_Halpha, l.F_Hbeta, l.EW_Hbeta, l.s2n_Hbeta, l.F_oiii, l.EW_oiii, l.s2n_oiii, l.F_nii, l.EW_nii, l.s2n_nii,r.RASS, r.SDSS, r.umag,r.gmag, r.rmag, r.imag, r.zmag, r.St, r.z, r.CRate, r.Exp, r.HR1, r.HR2, r.L, r.FX, r.g0mag, r.FXcor, r.logLX, r.LogL2500, r.logL2keV, r.alpha, r.Comm
FROM MyDB.Clustering_rosat_all AS r
INTO MyDB.Clustering_match_xray_starlight_01
    CROSS APPLY dbo.fGetNearestSpecObjEq(r.RA, r.DE, 1./60) AS n
    JOIN  MyDB.Clustering_STARLIGHT_lines_complete AS l ON (n.plate = l.plate AND n.mjd = l.mjd AND n.fiberid = l.fiberid)
