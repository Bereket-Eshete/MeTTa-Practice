# MeTTa-Practice

A collection of MeTTa list manipulation functions for learning functional programming concepts.

## Prerequisites

- **Linux** or **Windows WSL**
- Python 3.8+
- Git

## Setup

```bash
# Clone repository
git clone https://github.com/your-username/MeTTa-Practice.git
cd MeTTa-Practice

# Create and activate virtual environment
python3 -m venv metta-env
source metta-env/bin/activate

# Install MeTTa
pip install hyperon

# Run the code
metta listFunction.metta
```

## Functions

- **length** - Calculate list length
- **map** - Apply function to all elements  
- **foldl/foldr** - Reduce lists with accumulator
- **reverse** - Reverse list order
- **append** - Concatenate lists
- **filter** - Filter by predicate
- **take** - Take first n elements
- **member** - Check element membership

## Examples

```metta
!(length (cons 1 (cons 2 (cons 3 Nil))))           ; Result: 3
!(map square (cons 1 (cons 2 (cons 3 Nil))))       ; Result: [1, 4, 9]
!(reverse (cons 1 (cons 2 (cons 3 Nil))))          ; Result: [3, 2, 1]
```