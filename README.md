# Wind Turbine Critical System

By Benjamin Dodd (1901386)

## Description (Question A)

This program simulates a Wind Turbine braking system. A wind turbine has two types of braking systems:

- Aerodynamic brake: This is tip of the blade which can rotate to slow down the blades by air resistance.
- Mechanical brake: This uses a brake system on the generator itself that is only used as a backup, or to keep the turbine stopped for maintenance or adverse weather.
Wind Turbines each individually have an anemometer to measure wind speed at the turbine. This sensor reading is the input for this critical system.

As described in wind_turbine.ads, there are certain wind speeds that set the conditions of the brake systems.

If the wind speed is too low to induce inertia in the turbine, then both brake systems are applied.
While in a safe wind speed range, both brake systems are disengaged. If the wind approaches the maximum safe speed, the tip brakes are applied to begin slowing down the wind turbine.

Over the maximum safe speed, both brake systems are applied to prevent the wind turbine rotating over its safe speed. A failure at this stage could result in a generator fire, or even the blades shearing under stress.

The output of the system is the current state of the brake systems.

## Example inputs and outputs

Input: 0
Output: Both brakes applied

Input: 28
Output: Both brakes disengaged

Input: -4
Output: Please enter a valid wind speed

Input: 100
Output: Tip brake applied, mechanical brake disengaged

Input: 200
Output: Both brakes applied

Input: 300
Output: Please enter a valid wind speed
