import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm # For 3D color maps

# --- Simulation Parameters (must be the same as the numerical method for comparison) ---
nx = 51  # Number of points in the x direction
ny = 51  # Number of points in the y direction
nu = 0.01 # Viscosity coefficient (must be the same as the numerical method)

# Times for which the slices will be plotted.
# Adjust these values to match the times you want to compare with your numerical simulation.
# t=0.0 is the initial condition.
# The other times show the evolution.
times_for_slices = [0.2, 0.5, 1.0] # Example: initial time, intermediate time, final time

# --- Mesh Creation ---
x = np.linspace(0, 2, nx)
y = np.linspace(0, 2, ny)
X, Y = np.meshgrid(x, y) # 2D mesh for plotting

# --- Exact Solution Function (u component) ---
# This is a specific form of the exact solution for the 2D viscous Burgers' Equation.
# u(x, y, t) = -2 * nu * (d/dx (phi)) / phi
# v(x, y, t) = -2 * nu * (d/dy (phi)) / phi
# where phi is the solution of an associated 2D heat equation.

def phi_exact(x_coord, y_coord, t, nu):
    # Parameters for the specific form of the solution (adjustable for different profiles)
    # These values influence the initial shape of the "wave"
    A = 1.0
    B = 1.0
    C = 1.0
    D = 1.0

    # We add a small value to 't' to avoid division by zero at t=0 if the solution is evaluated there.
    # The '+1' in the denominator is a common convention to ensure the solution starts smoothly.
    # If you want the solution to start with a sharper "hat" at t=0, you can adjust this.
    t_effective = t + 1e-6 # Small offset for t=0

    return (A * np.exp((-(x_coord - 0.5)**2 - (y_coord - 0.5)**2) / (4 * nu * t_effective)) +
            B * np.exp((-(x_coord - 1.0)**2 - (y_coord - 1.0)**2) / (4 * nu * t_effective)) +
            C * np.exp((-(x_coord - 0.5)**2 - (y_coord - 1.0)**2) / (4 * nu * t_effective)) +
            D * np.exp((-(x_coord - 1.0)**2 - (y_coord - 0.5)**2) / (4 * nu * t_effective)))

def du_dx_exact(x_coord, y_coord, t, nu):
    t_effective = t + 1e-6
    # Partial derivatives of phi with respect to x
    term1 = -0.5 * (x_coord - 0.5) / (nu * t_effective) * np.exp((-(x_coord - 0.5)**2 - (y_coord - 0.5)**2) / (4 * nu * t_effective))
    term2 = -0.5 * (x_coord - 1.0) / (nu * t_effective) * np.exp((-(x_coord - 1.0)**2 - (y_coord - 1.0)**2) / (4 * nu * t_effective))
    term3 = -0.5 * (x_coord - 0.5) / (nu * t_effective) * np.exp((-(x_coord - 0.5)**2 - (y_coord - 1.0)**2) / (4 * nu * t_effective))
    term4 = -0.5 * (x_coord - 1.0) / (nu * t_effective) * np.exp((-(x_coord - 1.0)**2 - (y_coord - 0.5)**2) / (4 * nu * t_effective))
    return term1 + term2 + term3 + term4

def du_dy_exact(x_coord, y_coord, t, nu):
    t_effective = t + 1e-6
    # Partial derivatives of phi with respect to y
    term1 = -0.5 * (y_coord - 0.5) / (nu * t_effective) * np.exp((-(x_coord - 0.5)**2 - (y_coord - 0.5)**2) / (4 * nu * t_effective))
    term2 = -0.5 * (y_coord - 1.0) / (nu * t_effective) * np.exp((-(x_coord - 1.0)**2 - (y_coord - 1.0)**2) / (4 * nu * t_effective))
    term3 = -0.5 * (y_coord - 1.0) / (nu * t_effective) * np.exp((-(x_coord - 0.5)**2 - (y_coord - 1.0)**2) / (4 * nu * t_effective))
    term4 = -0.5 * (y_coord - 0.5) / (nu * t_effective) * np.exp((-(x_coord - 1.0)**2 - (y_coord - 0.5)**2) / (4 * nu * t_effective))
    return term1 + term2 + term3 + term4

# --- 2D Results Visualization (at the final time of the last slice) ---
# Calculate the exact solutions for u and v at the last slice time
t_final_plot_2d = times_for_slices[-1]
u_exact_2d = -2 * nu * (du_dx_exact(X, Y, t_final_plot_2d, nu) / phi_exact(X, Y, t_final_plot_2d, nu))
v_exact_2d = -2 * nu * (du_dy_exact(X, Y, t_final_plot_2d, nu) / phi_exact(X, Y, t_final_plot_2d, nu))

# Set figure size in inches and dpi to control image size
# 8 inches * 100 dpi = 800 pixels width
# 4 inches * 100 dpi = 400 pixels height
fig_2d = plt.figure(figsize=(8, 4), dpi=100)

# Plot of the u component in 2D
ax_u_2d = fig_2d.add_subplot(121, projection='3d')
ax_u_2d.plot_surface(X, Y, u_exact_2d, cmap=cm.viridis, rstride=1, cstride=1)
ax_u_2d.set_xlabel('X')
ax_u_2d.set_ylabel('Y')
ax_u_2d.set_zlabel('u Component (Exact)')
ax_u_2d.set_title(f'Exact Solution of u at t={t_final_plot_2d:.2f}')

# Plot of the v component in 2D
ax_v_2d = fig_2d.add_subplot(122, projection='3d')
ax_v_2d.plot_surface(X, Y, v_exact_2d, cmap=cm.plasma, rstride=1, cstride=1)
ax_v_2d.set_xlabel('X')
ax_v_2d.set_ylabel('Y')
ax_v_2d.set_zlabel('v Component (Exact)')
ax_v_2d.set_title(f'Exact Solution of v at t={t_final_plot_2d:.2f}')

plt.tight_layout()
plt.savefig('burgers_2d_solution.png')
plt.close(fig_2d) # Close the figure to free memory

# --- Slice Generation and Visualization ---
# Set figure size in inches and dpi to control image size
# 8 inches * 100 dpi = 800 pixels width
# 5 inches * 100 dpi = 500 pixels height
fig_slices = plt.figure(figsize=(8, 5), dpi=100)
ax_slices = fig_slices.add_subplot(111)

# Choose the slice in the middle of the Y domain
y_slice_index = ny // 2
y_slice_value = y[y_slice_index]

for t_current in times_for_slices:
    # Calculate the u_exact solution for the current time
    u_exact_at_t = -2 * nu * (du_dx_exact(X, Y, t_current, nu) / phi_exact(X, Y, t_current, nu))

    # Extract the slice of the u component
    u_slice = u_exact_at_t[y_slice_index, :]

    ax_slices.plot(x, u_slice, label=f't = {t_current:.2f}')

ax_slices.set_xlabel('X')
ax_slices.set_ylabel('u Component')
ax_slices.set_title(f'Slices of the Exact Solution of u (at Y={y_slice_value:.2f})')
ax_slices.legend()
ax_slices.grid(True)
plt.savefig('burgers_slices.png')
plt.close(fig_slices) # Close the figure to free memory

print(f"2D Exact Solution calculated for t = {t_final_plot_2d:.2f} and saved to 'burgers_2d_solution.png'")
print(f"Slices of the exact solution of u plotted for times: {times_for_slices} and saved to 'burgers_slices.png'")