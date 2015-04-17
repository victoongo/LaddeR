
# ./sparkR
# MASTER=<Spark master URL> ./sparkR

library(SparkR)
sc <- sparkR.init(master="local")
SPARK_MEM=1g ./sparkR


sparkR.stop()

./sparkR examples/pi.R local[2]

# R -e 'install.packages("testthat", repos="http://cran.us.r-project.org")'
install.packages('testthat')
./run-tests.sh
