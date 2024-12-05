# ---
# jupyter:
#   jupytext:
#     formats: ipynb,py:percent
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.17.2
#   kernelspec:
#     display_name: Python 3 (ipykernel)
#     language: python
#     name: python3
# ---

# %%
"""
This script calculates the exact solution for the viscous Burgers' equation.

The goal is to recreate the exact solution dataset `burgers_shock.mat`,
originally used in Raissi's work on PINNs (Physics-Informed Neural Networks).

The solution is based on John Burkardt's code and utilizes Gauss-Hermite
quadrature to approximate the integral representation of the solution, as
described by Basdevant et al. (1986).

Reference:
- Raissi, M., Perdikaris, P., & Karniadakis, G. E. (2019). Physics-informed neural
  networks: A deep learning framework for solving forward and inverse problems
  involving nonlinear partial differential equations. Journal of Computational
  Physics, 378, 686-707.
- Burkardt, J. (2015). Python code for Burgers' equation solution.
  https://people.sc.fsu.edu/~jburkardt/py_src/burgers_solution/burgers_solution.py
- Basdevant, C., et al. (1986). Spectral and finite difference solutions of the
  Burgers equation. Computers and Fluids, 14(1), 23-41.
"""

import numpy as np
import scipy.io
from scipy.special import gamma
from typing import Tuple


def imtqlx(n: int, d: np.ndarray, e: np.ndarray, z: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
    """
    Diagonalizes a symmetric tridiagonal matrix using the implicit QL algorithm.

    This is a modified version of the EISPACK routine to produce the product Q' * Z,
    where Z is an input vector and Q is the orthogonal matrix that diagonalizes the
    input matrix.

    Args:
        n: The order of the matrix.
        d: The diagonal entries of the matrix.
        e: The subdiagonal entries of the matrix (in e[0] to e[n-2]).
        z: A vector to be operated on.

    Returns:
        A tuple containing the eigenvalues and the vector Q' * Z.

    Raises:
        Exception: If the iteration limit is exceeded.
    """
    lam = np.copy(d)
    qtz = np.copy(z)

    if n == 1:
        return lam, qtz

    itn = 30
    epsilon = np.finfo(float).eps
    e[n - 1] = 0.0

    for l in range(n):
        j = 0
        while True:
            m = l
            while m < n:
                if m == n - 1:
                    break
                if abs(e[m]) <= epsilon * (abs(lam[m]) + abs(lam[m + 1])):
                    break
                m += 1

            p = lam[l]
            if m == l:
                break

            if itn <= j:
                raise Exception('imtqlx - Fatal error! Iteration limit exceeded.')

            j += 1
            g = (lam[l + 1] - p) / (2.0 * e[l])
            r = np.sqrt(g * g + 1.0)
            t = g - r if g < 0.0 else g + r
            g = lam[m] - p + e[l] / (g + t)

            s = 1.0
            c = 1.0
            p = 0.0

            for ii in range(m - 1, l - 1, -1):
                f = s * e[ii]
                b = c * e[ii]

                if abs(g) <= abs(f):
                    c = g / f
                    r = np.sqrt(c * c + 1.0)
                    e[ii + 1] = f * r
                    s = 1.0 / r
                    c = c * s
                else:
                    s = f / g
                    r = np.sqrt(s * s + 1.0)
                    e[ii + 1] = g * r
                    c = 1.0 / r
                    s = s * c

                g = lam[ii + 1] - p
                r = (lam[ii] - g) * s + 2.0 * c * b
                p = s * r
                lam[ii + 1] = g + p
                g = c * r - b
                f = qtz[ii + 1]
                qtz[ii + 1] = s * qtz[ii] + c * f
                qtz[ii] = c * qtz[ii] - s * f

            lam[l] = lam[l] - p
            e[l] = g
            e[m] = 0.0

    # Sort eigenvalues and eigenvectors
    for i in range(n - 1):
        k = i
        p = lam[i]
        for j in range(i + 1, n):
            if lam[j] < p:
                k = j
                p = lam[j]
        if k != i:
            lam[k] = lam[i]
            lam[i] = p
            p = qtz[i]
            qtz[i] = qtz[k]
            qtz[k] = p

    return lam, qtz


def hermite_ek_compute(n: int) -> Tuple[np.ndarray, np.ndarray]:
    """
    Calculates a Gauss-Hermite quadrature rule.

    The abscissas are the zeros of the N-th order Hermite polynomial.
    The integral is: integral(-oo, +oo) exp(-x^2) f(x) dx
    The quadrature rule is: sum(w_i * f(x_i))

    Args:
        n: The number of abscissas (order of the quadrature).

    Returns:
        A tuple containing the abscissas (x) and the weights (w).
    """
    zemu = gamma(0.5)
    bj = np.sqrt(np.arange(1, n + 1, dtype=float) / 2.0)
    
    x = np.zeros(n)
    w = np.zeros(n)
    w[0] = np.sqrt(zemu)

    x, w = imtqlx(n, x, bj[:-1], w)

    if n % 2 == 1:
        x[(n - 1) // 2] = 0.0

    w = w**2
    return x, w


def burgers_viscous_time_exact1(
    nu: float,
    vx: np.ndarray,
    vt: np.ndarray,
    qn: int = 50
) -> np.ndarray:
    """
    Evaluates a solution to the Burgers' equation using Gauss-Hermite quadrature.

    The form of the Burgers equation considered is:
      du/dt + u * du/dx = nu * d^2u/dx^2
    with initial conditions u(x,0) = -sin(pi*x) and boundary conditions u(-1,t) = u(+1,t) = 0.

    Args:
        nu: The viscosity coefficient.
        vx: Array of spatial grid points.
        vt: Array of temporal grid points.
        qn: The order of the Hermite quadrature rule.

    Returns:
        The solution u(x,t) of the Burgers' equation at each grid point.
    """
    print(f'  Quadrature order = {qn}')
    vxn = vx.shape[0]
    vtn = vt.shape[0]

    qx, qw = hermite_ek_compute(qn)
    vu = np.zeros((vxn, vtn))

    # Indices for t=0 and t>0
    t0_idx = np.where(vt == 0.0)[0]
    t_pos_idx = np.where(vt > 0.0)[0]

    # Case t=0: initial condition u(x,0) = -sin(pi*x)
    if t0_idx.size > 0:
        vu[:, t0_idx] = -np.sin(np.pi * vx[:, np.newaxis])

    # Case t>0: solution by quadrature
    if t_pos_idx.size > 0:
        t_pos = vt[t_pos_idx]

        # Reshape arrays for broadcasting
        vx_r = vx[:, np.newaxis, np.newaxis]
        t_r = t_pos[np.newaxis, :, np.newaxis]
        qx_r = qx[np.newaxis, np.newaxis, :]
        qw_r = qw[np.newaxis, np.newaxis, :]

        c = 2.0 * np.sqrt(nu * t_r)
        term1 = np.pi * (vx_r - c * qx_r)
        exp_term = np.exp(-np.cos(term1) / (2.0 * np.pi * nu))

        top = -np.sum(qw_r * c * np.sin(term1) * exp_term, axis=2)
        bot = np.sum(qw_r * c * exp_term, axis=2)

        # Avoid division by zero, although unlikely in this problem
        vu_pos = np.divide(top, bot, out=np.zeros_like(top), where=bot != 0)
        vu[:, t_pos_idx] = vu_pos

    return vu


def main():
    """
    Main function to generate the Burgers' equation solution and compare it
    with the reference data.
    """
    print("Starting the generation of the Burgers' equation solution...")

    # Parameters
    vtn = 100
    vxn = 256
    nu = 0.01 / np.pi
    xlo, xhi = -1.0, 1.0
    tlo, thi = 0.0, 0.99
    qn = 50 # Quadrature order

    print(f"  Viscosity NU = {nu}")
    print(f"  Spatial grid points NX = {vxn}")
    print(f"  Temporal grid points NT = {vtn}")

    # Create space and time grids
    vx = np.linspace(xlo, xhi, vxn)
    vt = np.linspace(tlo, thi, vtn)

    # Calculate the solution
    vu = burgers_viscous_time_exact1(nu, vx, vt, qn)

    # Save results to CSV files
    np.savetxt("burgers_solution_x.csv", vx, delimiter=",")
    np.savetxt("burgers_solution_t.csv", vt, delimiter=",")
    np.savetxt("burgers_solution_u.csv", vu, delimiter=",")
    print("
Files 'burgers_solution_x.csv', 'burgers_solution_t.csv', and "
          "'burgers_solution_u.csv' have been saved.")

    # Final check against the original .mat file
    print("
Checking the result against 'burgers_shock.mat'...")
    try:
        data = scipy.io.loadmat('../data/burgers_shock.mat')
        original = data['usol']
        if np.allclose(original, vu):
            print("  SUCCESS: The generated solution is compatible with the original.")
        else:
            print("  FAILURE: The generated solution differs from the original.")
    except FileNotFoundError:
        print("  WARNING: File '../data/burgers_shock.mat' not found.")
        print("  Could not perform the final check.")

    print("
Execution completed.")


if __name__ == '__main__':
    main()
