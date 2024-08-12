# Simulation of Conditioned Simple Random Walks

## Introduction
This project explores the concept of **Simple Random Walks** on the integer set $\mathbb{Z}$, inspired by the classical work of Karl Pearson in 1905. A random walk describes a path consisting of a succession of random steps, and it serves as a fundamental model in various fields such as physics, finance, and computer science.

## What is a Simple Random Walk?
A simple random walk on $\mathbb{Z}$ is a process where an entity starts at zero and takes steps to either $+1$ or $-1$ with equal probability. Each step is independent of the previous one, embodying the Markov property where the future position depends only on the current position.

## Conditioned Random Walks (Bridges)
This project focuses on **Conditioned Random Walks**, also known as **Bridges**. A bridge is a random walk that is conditioned to return to a specific point, typically the starting point, after a fixed number of steps $N$. Understanding these processes is key to analyzing constrained systems and their long-term behavior.

## Objective
The primary objective is to explore and compare various methods for simulating conditioned random walks, focusing on achieving both accuracy and computational efficiency.

## Simulation Methods
The project covers multiple simulation techniques:
- **Successive Sampling**: Based on the characterization of increments in a bridge.
- **Acceptance-Rejection**: Generates random walks and filters those that satisfy the bridge condition.
- **Markov Chain Simulation**: Uses Markov processes to inherently satisfy the bridge condition during simulation.

## Project Structure
1. **Section 1**: Introduction to random walks and their properties.
2. **Section 2**: Detailed exploration of the conditioned random walk (bridge) problem.
3. **Section 3**: Explanation and comparison of different simulation algorithms.
4. **Section 4**: Implementation of Markov chain methods for simulating bridges.
5. **Section 5**: Coupling methods for exact simulation according to uniform measures on the space of bridges.

