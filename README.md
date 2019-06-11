## GenArt:
GenArt is an R package that provides a genetic algorithm based on the principles of DNA replication with a certain error rate (mutation). 
The DNA is constructed of multiple polygons (coordinates, color codes, and transparencies) which results in an image similar to the source (or not) once 
they are drawn in a plot. If the mutated DNA is of better fitness compared to its source, it is kept for another mutation, if not, the source 
DNA is altered again, and so on and so forth... 

Main features are:

- cropping source image for better results
- construction of initial DNA (randomly)
- application of the actual algorithm
