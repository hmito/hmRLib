#' Stepper with Runge-Kutta Dormand-Prince5 method
#' @description This function returns integrated next step value by using the Runge-Kutta Dormand-Prince5 method
#' @param dxdt the function for derivative by dxdt(x,t)
#' @param x a number, vector or matrix
#' @param t time
#' @param dt step size
#' @return list; x: next step x, error: error
#' @export
#' @examples
#' x = c(0.5,0.3)
#' t = 0
#' dt = 0.01
#' dxdt = function(x,t){c(x[1]*(2-3*x[2]),-x[2]*(4-5*x[1]))}
#' lx1 = numeric(0)
#' lx2 = numeric(0)
#' lt = numeric(0)
#' while(t<10){
#' 	n = runge_kutta_dopri5_step(dxdt,x,t,dt)
#' 	x = n$x
#' 	t = n$t
#' 	dt = n$dt
#'
#' 	lx1 = c(lx1,x[1])
#' 	lx2 = c(lx2,x[2])
#' 	lt = c(lt,t)
#' }
#'
#' plot(lt,lx1,ylim=c(0,1.5))
#' points(lt,lx2,col="red")
runge_kutta_dopri5_step = function(dxdt, x, t, dt){
	k1 = dxdt(x,t)
	k2 = dxdt(x+1/4*k1*dt,t+dt/4)
	k3 = dxdt(x+(3*k1+9*k2)*dt/32,t+3*dt/8)
	k4 = dxdt(x+(1932*k1-7200*k2+7296*k3)*dt/2197,t+12*dt/13)
	k5 = dxdt(x+(439*k1/216-8*k2+3680/513*k3-845/4104*k4)*dt,t+dt)
	k6 = dxdt(x+(-8/27*k1+2*k2-3544/2565*k3+1859/4104*k4-11/40*k5)*dt,t+dt/2)

	x1 = x+dt*(25/216*k1+1408/2565*k3+2197/4104*k4-1/5*k5)
	x2 = x+dt*(16/135*k1+6656/12825*k3+28561/56430*k4-9/50*k5+2/55*k6)

	return(list(x=x1,t=t+dt, dt=dt, error=x2-x1))
}

#' Controlled stepper with Runge-Kutta Dormand-Prince5 method
#' @description This function returns integrated next step value by Dormand-Prince5 method with controling adaptive step with keeping errors smaller than given parameters
#' @param dxdt the function for derivative by dxdt(x,t)
#' @param x a number, vector or matrix
#' @param t time
#' @param dt step size
#' @param abserr permissible absolute error of calculation
#' @param relerr permissible relative error of calculation
#' @return list; x:a number, vector or matrix, t:time, dt:step size
#' @export
#' @examples
#' x = c(0.5,0.3)
#' t = 0
#' dt = 0.01
#' dxdt = function(x,t){c(x[1]*(2-3*x[2]),-x[2]*(4-5*x[1]))}
#' lx1 = numeric(0)
#' lx2 = numeric(0)
#' lt = numeric(0)
#' while(t<10){
#' 	n = runge_kutta_dopri5_adaptive_step(dxdt,x,t,dt,1e-6,1e-6)
#' 	x = n$x
#' 	t = n$t
#' 	dt = n$dt
#'
#' 	lx1 = c(lx1,x[1])
#' 	lx2 = c(lx2,x[2])
#' 	lt = c(lt,t)
#' }
#'
#' plot(lt,lx1,ylim=c(0,1.5))
#' points(lt,lx2,col="red")
#' #dt plot
#' plot(lt[-1]-lt[-length(lt)])
runge_kutta_dopri5_adaptive_step = function(dxdt, x,t,dt, abserr, relerr){
	err_order = 4
	step_order = 5

	while(T){
		ans = runge_kutta_dopri5_step(dxdt,x,t,dt)

		err  = max(abs(ans$error)/(abserr+relerr*(abs(x)+abs(ans$x-x)*dt)))
		if(err > 1){
			#decrease step
			dt = dt*max(9/10*err^(-1/(err_order-1)),1/5)
		}else{
			x = ans$x
			t = t+dt
			if(err<0.5){
				#try increase step
				err = max(5^(-step_order),err)
				dt = dt*(9/10*err^(-1/step_order))
			}
			return(list(x=x,t=t,dt=dt))
		}
	}
}
