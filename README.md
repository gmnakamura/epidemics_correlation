# epidemics_correlation
Monte Carlo simulation of agent-based SIS model


## Usage exemple
```shell
main -N 50 --steps 1000 --samples 10000 --gamma 0.3 --alpha 1.0 
```

## Data

Data is stored inside data*.dat files. Each row of the first column indicates the time step in the simulation. The remaining column represents a different statistics from the Monte Carlo simulation, averaged over samples informed in the command line.

$$t -- \langle \rho \rangle$$
