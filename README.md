# Executable Environment for OSF Project [kwp6n](https://osf.io/kwp6n/)

This repository was automatically generated as part of a project to test the reproducibility of open science projects hosted on the Open Science Framework (OSF).

**Project Title:** Trust in Everyday Life

**Project Description:**
> OSF project page for:

Weiss, A., Michels, C., Burgmer, P., Mussweiler, T., Ockenfels, A., &amp; Hofmann, W. (2021). Trust in everyday life. Journal of Personality and Social Psychology, 121, 95–114. https://doi.org/10.1037/pspi0000334

Abstract: 
Although trust plays a pivotal role in many aspects of life, very little is known about the manifestation of trust and distrust in everyday life. In this work, we integrated several prior approaches to trust and investigated the prevalence and key determinants of trust (vs. distrust) in people’s natural environments, using preregistered experience-sampling methodology. Across more than 4,500 social interactions from a heterogeneous sample of 427 participants, results showed high average levels of trust, but also considerable variability in trust across contexts. This variability was attributable to aspects of trustee perception, social distance, as well as 3 key dimensions of situational interdependence: conflict of interests, information (un)certainty, and power imbalance. At the dispositional level, average everyday trust was shaped by general trust, moral identity, and zero-sum beliefs. The social scope of most trust-related traits, however, was moderated by social distance: Whereas moral identity buffered against distrusting distant targets, high general distrust and low social value orientation amplified trust differences between close vs. distant others. Furthermore, a laboratory-based trust game predicted everyday trust only with regard to more distant but not close interaction partners. Finally, everyday trust was linked to self-disclosure and to cooperation, particularly in situations of high conflict between interaction partners’ interests. We conclude that trust can be conceptualized as a relational hub that interconnects the social perception of the trustee, the relational closeness between trustor and trustee, key structural features of situational interdependence, and behavioral response options such as self-disclosure.

**Original OSF Page:** [https://osf.io/kwp6n/](https://osf.io/kwp6n/)

---

**Important Note:** The contents of the `kwp6n_src` folder were cloned from the OSF project on **12-03-2025**. Any changes made to the original OSF project after this date will not be reflected in this repository.

The `DESCRIPTION` file was automatically added to make this project Binder-ready. For more information on how R-based OSF projects are containerized, please refer to the `osf-to-binder` GitHub repository: [https://github.com/Code-Inspect/osf-to-binder](https://github.com/Code-Inspect/osf-to-binder)

## flowR Integration

This version of the repository has the **[flowR Addin](https://github.com/flowr-analysis/rstudio-addin-flowr)** preinstalled. flowR allows visual design and execution of data analysis workflows within RStudio, supporting better reproducibility and modular analysis pipelines.

To use flowR, open the project in RStudio and go to `Addins` > `flowR`.

## How to Launch:

**Launch in your Browser:**

🚀 **MyBinder:** [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/code-inspect-binder/osf_kwp6n-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment in your web browser.
   * Please note that Binder may take a few minutes to build the environment.

🚀 **NFDI JupyterHub:** [![NFDI](https://nfdi-jupyter.de/images/nfdi_badge.svg)](https://hub.nfdi-jupyter.de/r2d/gh/code-inspect-binder/osf_kwp6n-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment on the NFDI JupyterHub platform.

**Access Downloaded Data:**
The downloaded data from the OSF project is located in the `kwp6n_src` folder.

## Run via Docker for Long-Term Reproducibility

In addition to launching this project using Binder or NFDI JupyterHub, you can reproduce the environment locally using Docker. This is especially useful for long-term access, offline use, or high-performance computing environments.

### Pull the Docker Image

```bash
docker pull meet261/repo2docker-kwp6n-f:latest
```

### Launch RStudio Server

Run the container (with a name, e.g. `rstudio-dev`):
```bash
docker run -it --name rstudio-dev --platform linux/amd64 -p 8888:8787 --user root meet261/repo2docker-kwp6n-f bash
```

Inside the container, start RStudio Server with no authentication:
```bash
/usr/lib/rstudio-server/bin/rserver --www-port 8787 --auth-none=1
```

Then, open your browser and go to: [http://localhost:8888](http://localhost:8888)

> **Note:** If you're running the container on a remote server (e.g., via SSH), replace `localhost` with your server's IP address.
> For example: `http://<your-server-ip>:8888`

## Looking for the Base Version?

For the original Binder-ready repository **without flowR**, visit:
[osf_kwp6n](https://github.com/code-inspect-binder/osf_kwp6n)

