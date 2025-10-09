# MITgcm_E_MAR_NhRbSlSc_T3d

MITgcm configuration to simulate Internal Solitary Waves in the Mascarene Ridge of the Indian Ocean, as documented in da Silva *et al.* (2015).  
👉 [https://doi.org/10.1016/j.dsr.2015.01.002](https://doi.org/10.1016/j.dsr.2015.01.002)

**E_MAR:** Experiment concerning the Mascarene Ridge  
**Nh:** Non-hydrostatic  
**Rb:** Real bathymetry  
**Sl:** Using Sponge Layers  
**Sc:** Using tides and an additional steady current  

---

## Contents
- `code/` – CPP options & patches  
- `build/` – genmake/build settings  
- `funcs/` – helper scripts  
- `data*`, `eedata`, etc. – runtime and input files  
- `bsub_mada6` – batch script (original HPC job)

---

## How to run
1. Build MITgcm for your system (`genmake2` + `make`).  
2. Copy or symlink `mitgcmuv`, or edit `bsub_mada6` to point to it.  
3. Adjust `data` and grid files if needed.  
4. Run `./mitgcmuv` (or submit your batch script).

---

## Input configuration

The input fields used in this configuration are derived from the *E_MAR* experiment family described in da Silva *et al.* (2015), tuned for the Mascarene Ridge region.  
All input files are provided as used in the original HPC experiment.  
Small numerical differences may arise on different architectures.

- **Bathymetry:** `topog.real_mada6b` – smoothed multibeam bathymetry over the Mascarene Ridge.  
- **Grid spacing:** `delX_mada6b`, `delY_mada6b` (if present) – horizontally uniform ≈ 1 km grid resolution.  
- **Initial stratification:** `data` and `eedata` define vertical profiles of temperature and salinity consistent with mooring and climatological profiles.  
- **Boundary conditions:** `data.obcs` – open boundary conditions using relaxed inflow/outflow from climatological fields.  
- **Wind forcing:** `data.pkg` includes `EXF` parameters corresponding to moderate monsoon wind conditions.  
- **Diagnostics:** `data.mnc` specifies MNC output fields (U, V, Θ, S, η) every few minutes for visualization and analysis.

---

## Acknowledgments
Original source: University of Southern Mississippi, Stennis Space Center — developed in collaboration with Prof. Maarten Buijsman.  

**Contact:** [jmagalhaes@fc.ul.pt](mailto:jmagalhaes@fc.ul.pt)

---

## References
da Silva, J. C. B., Buijsman, M. C., & Magalhães, J. M. R. (2015).  
*Internal solitary waves in the Mascarene Ridge region of the Indian Ocean.*  
**Deep-Sea Research Part I: Oceanographic Research Papers**, 99, 146–163.  
[https://doi.org/10.1016/j.dsr.2015.01.002](https://doi.org/10.1016/j.dsr.2015.01.002)
