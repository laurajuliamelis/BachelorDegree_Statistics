x=[0.25 0.50 0.75 1.00 1.25 1.50 1.75];y=[0.40 0.50 0.90 1.28 1.60 1.66 2.02];A=[x.^3',x.^2',x',ones(length(x),1)];rank(A);b = y;z = inv(A'*A) * A' * b'error = norm(b'-A*z)