######################################################################
# Filename   : linear_model.py
# Description:
# Author     : JL
# Created    : 2023-03-15 星期三 18:59:59 (+0800)
# LastUpdated:
# By         :
# Update #   : 3
######################################################################

### sympy 理论分析

from sympy import *
from sympy.abc import *
from sympy.stats import *
from IPython.display import display,Math
import pandas as pd

y=MatrixSymbol("y",n,1)
X=MatrixSymbol("X",n, p+1)
beta=MatrixSymbol("beta",p+1,1)
epsilon=MatrixSymbol("epsilon",n,1)

yi=Normal("y", mu, sigma)
density(yi)(z)

Math(r'f(x) := \frac{1}{\sigma\sqrt{2\pi}} e^{ -\frac{(x-\mu)^2}{2\sigma^2} }')

diff((y-X*beta).T*(y-X*beta), beta).expand()
betahat=(X.T*X)**(-1)*X.T*y

Math(r'\hat{\beta}='+latex(betahat))

trees=pd.read_csv("./data/trees.csv")

Xdata=trees.iloc[:,:2]
Xdata.insert(0,'cons',1)
ydata=trees.Volume

# betahat.subs({X:Matrix(Xdata),y:Matrix(ydata)}).doit() ## 有bug
((X.T*X)**(-1)*X.T).subs(X,Matrix(Xdata))*y.subs(y, Matrix(ydata))

######################################################################
## linear_model.py ends here
