import Pkg

packages = [
  "Revise"
]
Pkg.activate(".")
Pkg.add(packages)

using Revise
