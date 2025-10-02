# Exact Solution of the Viscous Burgers' Equation



This repository provides a Python script for computing the exact solution of the one-dimensional viscous Burgers' equation. The primary goal is to programmatically generate a high-fidelity dataset that replicates the `burgers_shock.mat` file used in the seminal work on Physics-Informed Neural Networks (PINNs) by Raissi, Perdikaris, and Karniadakis.

The implementation is a refactored and optimized version based on the original Fortran and Python codes by John Burkardt, which solve the equation using a Gauss-Hermite quadrature for a known integral representation of the solution.

## 1. Theoretical Background

The viscous Burgers' equation is a fundamental partial differential equation (PDE) that models processes involving convection and diffusion. It is particularly notable for its nonlinear convection term and its ability to form shock-like structures.

### The Equation

The one-dimensional form of the equation is:

$$
\frac{\partial u}{\partial t} + u \frac{\partial u}{\partial x} = \nu \frac{\partial^2 u}{\partial x^2}
$$

where:

- `u(x, t)` is the velocity field.
- `ν` (nu) is the viscosity coefficient.

For this specific problem, the following domain, initial condition (IC), and boundary conditions (BCs) are used, consistent with the reference paper:

- **Domain:** `x ∈ [-1, 1]`, `t ∈ [0, 1]`
- **Initial Condition:** `u(x, 0) = -sin(πx)`
- **Boundary Conditions:** `u(-1, t) = u(1, t) = 0`

### Method of Solution

The nonlinear Burgers' equation can be transformed into the linear heat equation using the **Cole-Hopf transformation**. This transformation is key to finding an analytical solution. While the script does not perform the transformation explicitly, it computes a solution derived from it.

As detailed by Basdevant et al. (1986), the solution `u(x, t)` can be expressed through an integral representation. This representation, however, is not trivial to evaluate directly. The approach implemented here uses **Gauss-Hermite quadrature** to numerically approximate the integral with high precision.

Gauss-Hermite quadrature is a powerful technique for approximating integrals of the form:

$$
\int_{-\infty}^{\infty} e^{-y^2} f(y) \,dy \approx \sum_{i=1}^{q} w_i f(y_i)
$$

where `y_i` are the roots (abscissas) of the Hermite polynomial of order `q`, and `w_i` are the corresponding weights. The script uses this method to evaluate the complex integral form of the solution at each point in the space-time grid.

## 2. Implementation Details

The file `burgers_solution.py` contains the full implementation. It is written in Python and relies on `NumPy` for efficient, vectorized numerical computation.

### Key Components

- **`hermite_ek_compute(n)`**: Computes the `n` abscissas and weights for the Gauss-Hermite quadrature rule using the Elhay-Kautsky algorithm.
- **`burgers_viscous_time_exact1(nu, vx, vt, qn)`**: This is the core function. It takes the viscosity (`nu`), spatial grid (`vx`), temporal grid (`vt`), and quadrature order (`qn`) as input. It calculates the solution `u(x, t)` over the entire grid using vectorized NumPy operations for performance, avoiding slow Python loops.
- **`main()`**: The main execution block that:
  1. Sets the physical and numerical parameters (e.g., `ν = 0.01/π`, grid points `vxn=256`, `vtn=100`).
  2. Initializes the spatial and temporal grids.
  3. Calls the solver function to generate the solution matrix.
  4. Saves the output to CSV files.
  5. Performs a verification check against the reference `.mat` file.

### Parameters

The key parameters are defined in the `main()` function:

- `vtn = 100`: Number of points in the temporal grid.
- `vxn = 256`: Number of points in the spatial grid.
- `nu = 0.01 / np.pi`: Viscosity coefficient.
- `qn = 50`: Order of the Gauss-Hermite quadrature. A higher order yields greater accuracy.

## 3. Usage

To generate the solution dataset, simply run the script from the command line:

```bash
python3 burgers_solution.py
```

### Prerequisites

You need Python 3 with the following libraries installed:

- `numpy`
- `scipy`

You can install them using pip:

```bash
pip install numpy scipy
```

### Outputs

The script will produce three files in the current directory:

- `burgers_solution_x.csv`: The spatial grid points `x`.
- `burgers_solution_t.csv`: The temporal grid points `t`.
- `burgers_solution_u.csv`: The solution matrix `u(x, t)`, where each row corresponds to a spatial point and each column to a time step.

## 4. Verification

After generating the solution, the script attempts to verify its correctness by comparing it to the original `burgers_shock.mat` file provided by Raissi et al.

For this check to work, the `burgers_shock.mat` file must be located in a directory named `data` at the parent level of this repository (`../data/burgers_shock.mat`).

The verification uses `numpy.allclose()`, which checks if the generated solution and the reference solution are element-wise equal within a small tolerance. The script will print a `SUCCESS` or `FAILURE` message to the console.

## 5. References

1. **Raissi, M., Perdikaris, P., & Karniadakis, G. E. (2019).** *Physics-informed neural networks: A deep learning framework for solving forward and inverse problems involving nonlinear partial differential equations.* Journal of Computational Physics, 378, 686-707. [Link to paper](https://www.sciencedirect.com/science/article/pii/S002199911830755X)
2. **Burkardt, J. (2015).** *BURGERS_SOLUTION: Python code for the exact solution of the time-dependent 1D viscous Burgers equation.* [FSU Website](https://people.sc.fsu.edu/~jburkardt/py_src/burgers_solution/burgers_solution.html)
3. **Basdevant, C., et al. (1986).** *Spectral and finite difference solutions of the Burgers equation.* Computers and Fluids, 14(1), 23-41.
<br><sub>Last edited: 2025-08-02 19:46:03</sub>
