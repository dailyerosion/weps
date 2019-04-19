!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine slsoda (isr, f, neq, y, t, tout, itol, rtol, atol,     &
     &            itask, istate, iopt, rwork, lrw, iwork, liw, jac, jt)

      use hydro_darcy_mod, only: sls1, slsa, sloc

      external f, jac
      integer isr, neq, itol, itask, istate, iopt, lrw, iwork, liw, jt
      real y, t, tout, rtol, atol, rwork
      dimension neq(*), y(*), rtol(*), atol(*), rwork(lrw), iwork(liw)
!-----------------------------------------------------------------------
! This is the 25 April 2001 version of
! SLSODA: Livermore Solver for Ordinary Differential Equations, with
!         Automatic method switching for stiff and nonstiff problems.
!
! This version is in single precision.
!
! SLSODA solves the initial value problem for stiff or nonstiff
! systems of first order ODEs,
!     dy/dt = f(t,y) ,  or, in component form,
!     dy(i)/dt = f(i) = f(i,t,y(1),y(2),...,y(NEQ)) (i = 1,...,NEQ).
!
! This a variant version of the SLSODE package.
! It switches automatically between stiff and nonstiff methods.
! This means that the user does not have to determine whether the
! problem is stiff or not, and the solver will automatically choose the
! appropriate method.  It always starts with the nonstiff method.
!
! Authors:       Alan C. Hindmarsh
!                Center for Applied Scientific Computing, L-561
!                Lawrence Livermore National Laboratory
!                Livermore, CA 94551
! and
!                Linda R. Petzold
!                Univ. of California at Santa Barbara
!                Dept. of Computer Science
!                Santa Barbara, CA 93106
!
! References:
! 1.  Alan C. Hindmarsh,  ODEPACK, A Systematized Collection of ODE
!     Solvers, in Scientific Computing, R. S. Stepleman et al. (Eds.),
!     North-Holland, Amsterdam, 1983, pp. 55-64.
! 2.  Linda R. Petzold, Automatic Selection of Methods for Solving
!     Stiff and Nonstiff Systems of Ordinary Differential Equations,
!     Siam J. Sci. Stat. Comput. 4 (1983), pp. 136-148.
!-----------------------------------------------------------------------
! Summary of Usage.
!
! Communication between the user and the SLSODA package, for normal
! situations, is summarized here.  This summary describes only a subset
! of the full set of options available.  See the full description for
! details, including alternative treatment of the Jacobian matrix,
! optional inputs and outputs, nonstandard options, and
! instructions for special situations.  See also the example
! problem (with program and output) following this summary.
!
! A. First provide a subroutine of the form:
!               SUBROUTINE F (NEQ, T, Y, YDOT)
!               DIMENSION Y(*), YDOT(*)
! which supplies the vector function f by loading YDOT(i) with f(i).
!
! B. Write a main program which calls Subroutine SLSODA once for
! each point at which answers are desired.  This should also provide
! for possible use of logical unit 6 for output of error messages
! by SLSODA.  On the first call to SLSODA, supply arguments as follows:
! F      = name of subroutine for right-hand side vector f.
!          This name must be declared External in calling program.
! NEQ    = number of first order ODEs.
! Y      = array of initial values, of length NEQ.
! T      = the initial value of the independent variable.
! TOUT   = first point where output is desired (.ne. T).
! ITOL   = 1 or 2 according as ATOL (below) is a scalar or array.
! RTOL   = relative tolerance parameter (scalar).
! ATOL   = absolute tolerance parameter (scalar or array).
!          the estimated local error in y(i) will be controlled so as
!          to be less than
!             EWT(i) = RTOL*ABS(Y(i)) + ATOL     if ITOL = 1, or
!             EWT(i) = RTOL*ABS(Y(i)) + ATOL(i)  if ITOL = 2.
!          Thus the local error test passes if, in each component,
!          either the absolute error is less than ATOL (or ATOL(i)),
!          or the relative error is less than RTOL.
!          Use RTOL = 0.0 for pure absolute error control, and
!          use ATOL = 0.0 (or ATOL(i) = 0.0) for pure relative error
!          control.  Caution: actual (global) errors may exceed these
!          local tolerances, so choose them conservatively.
! ITASK  = 1 for normal computation of output values of y at t = TOUT.
! ISTATE = integer flag (input and output).  Set ISTATE = 1.
! IOPT   = 0 to indicate no optional inputs used.
! RWORK  = real work array of length at least:
!             22 + NEQ * MAX(16, NEQ + 9).
!          See also Paragraph E below.
! LRW    = declared length of RWORK (in user's dimension).
! IWORK  = integer work array of length at least  20 + NEQ.
! LIW    = declared length of IWORK (in user's dimension).
! JAC    = name of subroutine for Jacobian matrix.
!          Use a dummy name.  See also Paragraph E below.
! JT     = Jacobian type indicator.  Set JT = 2.
!          See also Paragraph E below.
! Note that the main program must declare arrays Y, RWORK, IWORK,
! and possibly ATOL.
!
! C. The output from the first call (or any call) is:
!      Y = array of computed values of y(t) vector.
!      T = corresponding value of independent variable (normally TOUT).
! ISTATE = 2  if SLSODA was successful, negative otherwise.
!          -1 means excess work done on this call (perhaps wrong JT).
!          -2 means excess accuracy requested (tolerances too small).
!          -3 means illegal input detected (see printed message).
!          -4 means repeated error test failures (check all inputs).
!          -5 means repeated convergence failures (perhaps bad Jacobian
!             supplied or wrong choice of JT or tolerances).
!          -6 means error weight became zero during problem. (Solution
!             component i vanished, and ATOL or ATOL(i) = 0.)
!          -7 means work space insufficient to finish (see messages).
!
! D. To continue the integration after a successful return, simply
! reset TOUT and call SLSODA again.  No other parameters need be reset.
!
! E. Note: If and when SLSODA regards the problem as stiff, and
! switches methods accordingly, it must make use of the NEQ by NEQ
! Jacobian matrix, J = df/dy.  For the sake of simplicity, the
! inputs to SLSODA recommended in Paragraph B above cause SLSODA to
! treat J as a full matrix, and to approximate it internally by
! difference quotients.  Alternatively, J can be treated as a band
! matrix (with great potential reduction in the size of the RWORK
! array).  Also, in either the full or banded case, the user can supply
! J in closed form, with a routine whose name is passed as the JAC
! argument.  These alternatives are described in the paragraphs on
! RWORK, JAC, and JT in the full description of the call sequence below.
!
!-----------------------------------------------------------------------
! Example Problem.
!
! The following is a simple example problem, with the coding
! needed for its solution by SLSODA.  The problem is from chemical
! kinetics, and consists of the following three rate equations:
!     dy1/dt = -.04*y1 + 1.e4*y2*y3
!     dy2/dt = .04*y1 - 1.e4*y2*y3 - 3.e7*y2**2
!     dy3/dt = 3.e7*y2**2
! on the interval from t = 0.0 to t = 4.e10, with initial conditions
! y1 = 1.0, y2 = y3 = 0.  The problem is stiff.
!
! The following coding solves this problem with SLSODA,
! printing results at t = .4, 4., ..., 4.e10.  It uses
! ITOL = 2 and ATOL much smaller for y2 than y1 or y3 because
! y2 has much smaller values.
! At the end of the run, statistical quantities of interest are
! printed (see optional outputs in the full description below).
!
!     EXTERNAL FEX
!     REAL ATOL, RTOL, RWORK, T, TOUT, Y
!     DIMENSION Y(3), ATOL(3), RWORK(70), IWORK(23)
!     NEQ = 3
!     Y(1) = 1.
!     Y(2) = 0.
!     Y(3) = 0.
!     T = 0.
!     TOUT = .4
!     ITOL = 2
!     RTOL = 1.E-4
!     ATOL(1) = 1.E-6
!     ATOL(2) = 1.E-10
!     ATOL(3) = 1.E-6
!     ITASK = 1
!     ISTATE = 1
!     IOPT = 0
!     LRW = 70
!     LIW = 23
!     JT = 2
!     DO 40 IOUT = 1,12
!       CALL SLSODA(FEX,NEQ,Y,T,TOUT,ITOL,RTOL,ATOL,ITASK,ISTATE,
!    1     IOPT,RWORK,LRW,IWORK,LIW,JDUM,JT)
!       WRITE(6,20)T,Y(1),Y(2),Y(3)
! 20    FORMAT(' At t =',E12.4,'   Y =',3E14.6)
!       IF (ISTATE .LT. 0) GO TO 80
! 40    TOUT = TOUT*10.
!     WRITE(6,60)IWORK(11),IWORK(12),IWORK(13),IWORK(19),RWORK(15)
! 60  FORMAT(/' No. steps =',I4,'  No. f-s =',I4,'  No. J-s =',I4/
!    1   ' Method last used =',I2,'   Last switch was at t =',E12.4)
!     STOP
! 80  WRITE(6,90)ISTATE
! 90  FORMAT(///' Error halt.. ISTATE =',I3)
!     STOP
!     END
!
!     SUBROUTINE FEX (NEQ, T, Y, YDOT)
!     REAL T, Y, YDOT
!     DIMENSION Y(3), YDOT(3)
!     YDOT(1) = -.04*Y(1) + 1.E4*Y(2)*Y(3)
!     YDOT(3) = 3.E7*Y(2)*Y(2)
!     YDOT(2) = -YDOT(1) - YDOT(3)
!     RETURN
!     END
!
! The output of this program (on a CDC-7600 in single precision)
! is as follows:
!
!   At t =  4.0000e-01   y =  9.851712e-01  3.386380e-05  1.479493e-02
!   At t =  4.0000e+00   Y =  9.055333e-01  2.240655e-05  9.444430e-02
!   At t =  4.0000e+01   Y =  7.158403e-01  9.186334e-06  2.841505e-01
!   At t =  4.0000e+02   Y =  4.505250e-01  3.222964e-06  5.494717e-01
!   At t =  4.0000e+03   Y =  1.831975e-01  8.941774e-07  8.168016e-01
!   At t =  4.0000e+04   Y =  3.898730e-02  1.621940e-07  9.610125e-01
!   At t =  4.0000e+05   Y =  4.936363e-03  1.984221e-08  9.950636e-01
!   At t =  4.0000e+06   Y =  5.161831e-04  2.065786e-09  9.994838e-01
!   At t =  4.0000e+07   Y =  5.179817e-05  2.072032e-10  9.999482e-01
!   At t =  4.0000e+08   Y =  5.283401e-06  2.113371e-11  9.999947e-01
!   At t =  4.0000e+09   Y =  4.659031e-07  1.863613e-12  9.999995e-01
!   At t =  4.0000e+10   Y =  1.404280e-08  5.617126e-14  1.000000e+00
!
!   No. steps = 361  No. f-s = 693  No. J-s =  64
!   Method last used = 2   Last switch was at t =  6.0092e-03
!-----------------------------------------------------------------------
! Full description of user interface to SLSODA.
!
! The user interface to SLSODA consists of the following parts.
!
! 1.   The call sequence to Subroutine SLSODA, which is a driver
!      routine for the solver.  This includes descriptions of both
!      the call sequence arguments and of user-supplied routines.
!      following these descriptions is a description of
!      optional inputs available through the call sequence, and then
!      a description of optional outputs (in the work arrays).
!
! 2.   Descriptions of other routines in the SLSODA package that may be
!      (optionally) called by the user.  These provide the ability to
!      alter error message handling, save and restore the internal
!      Common, and obtain specified derivatives of the solution y(t).
!
! 3.   Descriptions of Common blocks to be declared in overlay
!      or similar environments, or to be saved when doing an interrupt
!      of the problem and continued solution later.
!
! 4.   Description of a subroutine in the SLSODA package,
!      which the user may replace with his/her own version, if desired.
!      this relates to the measurement of errors.
!
!-----------------------------------------------------------------------
! Part 1.  Call Sequence.
!
! The call sequence parameters used for input only are
!     F, NEQ, TOUT, ITOL, RTOL, ATOL, ITASK, IOPT, LRW, LIW, JAC, JT,
! and those used for both input and output are
!     Y, T, ISTATE.
! The work arrays RWORK and IWORK are also used for conditional and
! optional inputs and optional outputs.  (The term output here refers
! to the return from Subroutine SLSODA to the user's calling program.)
!
! The legality of input parameters will be thoroughly checked on the
! initial call for the problem, but not checked thereafter unless a
! change in input parameters is flagged by ISTATE = 3 on input.
!
! The descriptions of the call arguments are as follows.
!
! F      = the name of the user-supplied subroutine defining the
!          ODE system.  The system must be put in the first-order
!          form dy/dt = f(t,y), where f is a vector-valued function
!          of the scalar t and the vector y.  Subroutine F is to
!          compute the function f.  It is to have the form
!               SUBROUTINE F (NEQ, T, Y, YDOT)
!               DIMENSION Y(*), YDOT(*)
!          where NEQ, T, and Y are input, and the array YDOT = f(t,y)
!          is output.  Y and YDOT are arrays of length NEQ.
!          Subroutine F should not alter Y(1),...,Y(NEQ).
!          F must be declared External in the calling program.
!
!          Subroutine F may access user-defined quantities in
!          NEQ(2),... and/or in Y(NEQ(1)+1),... if NEQ is an array
!          (dimensioned in F) and/or Y has length exceeding NEQ(1).
!          See the descriptions of NEQ and Y below.
!
!          If quantities computed in the F routine are needed
!          externally to SLSODA, an extra call to F should be made
!          for this purpose, for consistent and accurate results.
!          If only the derivative dy/dt is needed, use SINTDY instead.
!
! NEQ    = the size of the ODE system (number of first order
!          ordinary differential equations).  Used only for input.
!          NEQ may be decreased, but not increased, during the problem.
!          If NEQ is decreased (with ISTATE = 3 on input), the
!          remaining components of Y should be left undisturbed, if
!          these are to be accessed in F and/or JAC.
!
!          Normally, NEQ is a scalar, and it is generally referred to
!          as a scalar in this user interface description.  However,
!          NEQ may be an array, with NEQ(1) set to the system size.
!          (The SLSODA package accesses only NEQ(1).)  In either case,
!          this parameter is passed as the NEQ argument in all calls
!          to F and JAC.  Hence, if it is an array, locations
!          NEQ(2),... may be used to store other integer data and pass
!          it to F and/or JAC.  Subroutines F and/or JAC must include
!          NEQ in a Dimension statement in that case.
!
! Y      = a real array for the vector of dependent variables, of
!          length NEQ or more.  Used for both input and output on the
!          first call (ISTATE = 1), and only for output on other calls.
!          On the first call, Y must contain the vector of initial
!          values.  On output, Y contains the computed solution vector,
!          evaluated at T.  If desired, the Y array may be used
!          for other purposes between calls to the solver.
!
!          This array is passed as the Y argument in all calls to
!          F and JAC.  Hence its length may exceed NEQ, and locations
!          Y(NEQ+1),... may be used to store other real data and
!          pass it to F and/or JAC.  (The SLSODA package accesses only
!          Y(1),...,Y(NEQ).)
!
! T      = the independent variable.  On input, T is used only on the
!          first call, as the initial point of the integration.
!          on output, after each call, T is the value at which a
!          computed solution Y is evaluated (usually the same as TOUT).
!          on an error return, T is the farthest point reached.
!
! TOUT   = the next value of t at which a computed solution is desired.
!          Used only for input.
!
!          When starting the problem (ISTATE = 1), TOUT may be equal
!          to T for one call, then should .ne. T for the next call.
!          For the initial t, an input value of TOUT .ne. T is used
!          in order to determine the direction of the integration
!          (i.e. the algebraic sign of the step sizes) and the rough
!          scale of the problem.  Integration in either direction
!          (forward or backward in t) is permitted.
!
!          If ITASK = 2 or 5 (one-step modes), TOUT is ignored after
!          the first call (i.e. the first call with TOUT .ne. T).
!          Otherwise, TOUT is required on every call.
!
!          If ITASK = 1, 3, or 4, the values of TOUT need not be
!          monotone, but a value of TOUT which backs up is limited
!          to the current internal T interval, whose endpoints are
!          TCUR - HU and TCUR (see optional outputs, below, for
!          TCUR and HU).
!
! ITOL   = an indicator for the type of error control.  See
!          description below under ATOL.  Used only for input.
!
! RTOL   = a relative error tolerance parameter, either a scalar or
!          an array of length NEQ.  See description below under ATOL.
!          Input only.
!
! ATOL   = an absolute error tolerance parameter, either a scalar or
!          an array of length NEQ.  Input only.
!
!             The input parameters ITOL, RTOL, and ATOL determine
!          the error control performed by the solver.  The solver will
!          control the vector E = (E(i)) of estimated local errors
!          in y, according to an inequality of the form
!                      max-norm of ( E(i)/EWT(i) )   .le.   1,
!          where EWT = (EWT(i)) is a vector of positive error weights.
!          The values of RTOL and ATOL should all be non-negative.
!          The following table gives the types (scalar/array) of
!          RTOL and ATOL, and the corresponding form of EWT(i).
!
!             ITOL    RTOL       ATOL          EWT(i)
!              1     scalar     scalar     RTOL*ABS(Y(i)) + ATOL
!              2     scalar     array      RTOL*ABS(Y(i)) + ATOL(i)
!              3     array      scalar     RTOL(i)*ABS(Y(i)) + ATOL
!              4     array      array      RTOL(i)*ABS(Y(i)) + ATOL(i)
!
!          When either of these parameters is a scalar, it need not
!          be dimensioned in the user's calling program.
!
!          If none of the above choices (with ITOL, RTOL, and ATOL
!          fixed throughout the problem) is suitable, more general
!          error controls can be obtained by substituting
!          a user-supplied routine for the setting of EWT.
!          See Part 4 below.
!
!          If global errors are to be estimated by making a repeated
!          run on the same problem with smaller tolerances, then all
!          components of RTOL and ATOL (i.e. of EWT) should be scaled
!          down uniformly.
!
! ITASK  = an index specifying the task to be performed.
!          Input only.  ITASK has the following values and meanings.
!          1  means normal computation of output values of y(t) at
!             t = TOUT (by overshooting and interpolating).
!          2  means take one step only and return.
!          3  means stop at the first internal mesh point at or
!             beyond t = TOUT and return.
!          4  means normal computation of output values of y(t) at
!             t = TOUT but without overshooting t = TCRIT.
!             TCRIT must be input as RWORK(1).  TCRIT may be equal to
!             or beyond TOUT, but not behind it in the direction of
!             integration.  This option is useful if the problem
!             has a singularity at or beyond t = TCRIT.
!          5  means take one step, without passing TCRIT, and return.
!             TCRIT must be input as RWORK(1).
!
!          Note:  If ITASK = 4 or 5 and the solver reaches TCRIT
!          (within roundoff), it will return T = TCRIT (exactly) to
!          indicate this (unless ITASK = 4 and TOUT comes before TCRIT,
!          in which case answers at t = TOUT are returned first).
!
! ISTATE = an index used for input and output to specify the
!          the state of the calculation.
!
!          On input, the values of ISTATE are as follows.
!          1  means this is the first call for the problem
!             (initializations will be done).  See note below.
!          2  means this is not the first call, and the calculation
!             is to continue normally, with no change in any input
!             parameters except possibly TOUT and ITASK.
!             (If ITOL, RTOL, and/or ATOL are changed between calls
!             with ISTATE = 2, the new values will be used but not
!             tested for legality.)
!          3  means this is not the first call, and the
!             calculation is to continue normally, but with
!             a change in input parameters other than
!             TOUT and ITASK.  Changes are allowed in
!             NEQ, ITOL, RTOL, ATOL, IOPT, LRW, LIW, JT, ML, MU,
!             and any optional inputs except H0, MXORDN, and MXORDS.
!             (See IWORK description for ML and MU.)
!          Note:  A preliminary call with TOUT = T is not counted
!          as a first call here, as no initialization or checking of
!          input is done.  (Such a call is sometimes useful for the
!          purpose of outputting the initial conditions.)
!          Thus the first call for which TOUT .ne. T requires
!          ISTATE = 1 on input.
!
!          On output, ISTATE has the following values and meanings.
!           1  means nothing was done; TOUT = T and ISTATE = 1 on input.
!           2  means the integration was performed successfully.
!          -1  means an excessive amount of work (more than MXSTEP
!              steps) was done on this call, before completing the
!              requested task, but the integration was otherwise
!              successful as far as T.  (MXSTEP is an optional input
!              and is normally 500.)  To continue, the user may
!              simply reset ISTATE to a value .gt. 1 and call again
!              (the excess work step counter will be reset to 0).
!              In addition, the user may increase MXSTEP to avoid
!              this error return (see below on optional inputs).
!          -2  means too much accuracy was requested for the precision
!              of the machine being used.  This was detected before
!              completing the requested task, but the integration
!              was successful as far as T.  To continue, the tolerance
!              parameters must be reset, and ISTATE must be set
!              to 3.  The optional output TOLSF may be used for this
!              purpose.  (Note: If this condition is detected before
!              taking any steps, then an illegal input return
!              (ISTATE = -3) occurs instead.)
!          -3  means illegal input was detected, before taking any
!              integration steps.  See written message for details.
!              Note:  If the solver detects an infinite loop of calls
!              to the solver with illegal input, it will cause
!              the run to stop.
!          -4  means there were repeated error test failures on
!              one attempted step, before completing the requested
!              task, but the integration was successful as far as T.
!              The problem may have a singularity, or the input
!              may be inappropriate.
!          -5  means there were repeated convergence test failures on
!              one attempted step, before completing the requested
!              task, but the integration was successful as far as T.
!              This may be caused by an inaccurate Jacobian matrix,
!              if one is being used.
!          -6  means EWT(i) became zero for some i during the
!              integration.  Pure relative error control (ATOL(i)=0.0)
!              was requested on a variable which has now vanished.
!              The integration was successful as far as T.
!          -7  means the length of RWORK and/or IWORK was too small to
!              proceed, but the integration was successful as far as T.
!              This happens when SLSODA chooses to switch methods
!              but LRW and/or LIW is too small for the new method.
!
!          Note:  Since the normal output value of ISTATE is 2,
!          it does not need to be reset for normal continuation.
!          Also, since a negative input value of ISTATE will be
!          regarded as illegal, a negative output value requires the
!          user to change it, and possibly other inputs, before
!          calling the solver again.
!
! IOPT   = an integer flag to specify whether or not any optional
!          inputs are being used on this call.  Input only.
!          The optional inputs are listed separately below.
!          IOPT = 0 means no optional inputs are being used.
!                   default values will be used in all cases.
!          IOPT = 1 means one or more optional inputs are being used.
!
! RWORK  = a real array (single precision) for work space, and (in the
!          first 20 words) for conditional and optional inputs and
!          optional outputs.
!          As SLSODA switches automatically between stiff and nonstiff
!          methods, the required length of RWORK can change during the
!          problem.  Thus the RWORK array passed to SLSODA can either
!          have a static (fixed) length large enough for both methods,
!          or have a dynamic (changing) length altered by the calling
!          program in response to output from SLSODA.
!
!                       --- Fixed Length Case ---
!          If the RWORK length is to be fixed, it should be at least
!               MAX (LRN, LRS),
!          where LRN and LRS are the RWORK lengths required when the
!          current method is nonstiff or stiff, respectively.
!
!          The separate RWORK length requirements LRN and LRS are
!          as follows:
!          IF NEQ is constant and the maximum method orders have
!          their default values, then
!             LRN = 20 + 16*NEQ,
!             LRS = 22 + 9*NEQ + NEQ**2           if JT = 1 or 2,
!             LRS = 22 + 10*NEQ + (2*ML+MU)*NEQ   if JT = 4 or 5.
!          Under any other conditions, LRN and LRS are given by:
!             LRN = 20 + NYH*(MXORDN+1) + 3*NEQ,
!             LRS = 20 + NYH*(MXORDS+1) + 3*NEQ + LMAT,
!          where
!             NYH    = the initial value of NEQ,
!             MXORDN = 12, unless a smaller value is given as an
!                      optional input,
!             MXORDS = 5, unless a smaller value is given as an
!                      optional input,
!             LMAT   = length of matrix work space:
!             LMAT   = NEQ**2 + 2              if JT = 1 or 2,
!             LMAT   = (2*ML + MU + 1)*NEQ + 2 if JT = 4 or 5.
!
!                       --- Dynamic Length Case ---
!          If the length of RWORK is to be dynamic, then it should
!          be at least LRN or LRS, as defined above, depending on the
!          current method.  Initially, it must be at least LRN (since
!          SLSODA starts with the nonstiff method).  On any return
!          from SLSODA, the optional output MCUR indicates the current
!          method.  If MCUR differs from the value it had on the
!          previous return, or if there has only been one call to
!          SLSODA and MCUR is now 2, then SLSODA has switched
!          methods during the last call, and the length of RWORK
!          should be reset (to LRN if MCUR = 1, or to LRS if
!          MCUR = 2).  (An increase in the RWORK length is required
!          if SLSODA returned ISTATE = -7, but not otherwise.)
!          After resetting the length, call SLSODA with ISTATE = 3
!          to signal that change.
!
! LRW    = the length of the array RWORK, as declared by the user.
!          (This will be checked by the solver.)
!
! IWORK  = an integer array for work space.
!          As SLSODA switches automatically between stiff and nonstiff
!          methods, the required length of IWORK can change during
!          problem, between
!             LIS = 20 + NEQ   and   LIN = 20,
!          respectively.  Thus the IWORK array passed to SLSODA can
!          either have a fixed length of at least 20 + NEQ, or have a
!          dynamic length of at least LIN or LIS, depending on the
!          current method.  The comments on dynamic length under
!          RWORK above apply here.  Initially, this length need
!          only be at least LIN = 20.
!
!          The first few words of IWORK are used for conditional and
!          optional inputs and optional outputs.
!
!          The following 2 words in IWORK are conditional inputs:
!            IWORK(1) = ML     these are the lower and upper
!            IWORK(2) = MU     half-bandwidths, respectively, of the
!                       banded Jacobian, excluding the main diagonal.
!                       The band is defined by the matrix locations
!                       (i,j) with i-ML .le. j .le. i+MU.  ML and MU
!                       must satisfy  0 .le.  ML,MU  .le. NEQ-1.
!                       These are required if JT is 4 or 5, and
!                       ignored otherwise.  ML and MU may in fact be
!                       the band parameters for a matrix to which
!                       df/dy is only approximately equal.
!
! LIW    = the length of the array IWORK, as declared by the user.
!          (This will be checked by the solver.)
!
! Note: The base addresses of the work arrays must not be
! altered between calls to SLSODA for the same problem.
! The contents of the work arrays must not be altered
! between calls, except possibly for the conditional and
! optional inputs, and except for the last 3*NEQ words of RWORK.
! The latter space is used for internal scratch space, and so is
! available for use by the user outside SLSODA between calls, if
! desired (but not for use by F or JAC).
!
! JAC    = the name of the user-supplied routine to compute the
!          Jacobian matrix, df/dy, if JT = 1 or 4.  The JAC routine
!          is optional, but if the problem is expected to be stiff much
!          of the time, you are encouraged to supply JAC, for the sake
!          of efficiency.  (Alternatively, set JT = 2 or 5 to have
!          SLSODA compute df/dy internally by difference quotients.)
!          If and when SLSODA uses df/dy, it treats this NEQ by NEQ
!          matrix either as full (JT = 1 or 2), or as banded (JT =
!          4 or 5) with half-bandwidths ML and MU (discussed under
!          IWORK above).  In either case, if JT = 1 or 4, the JAC
!          routine must compute df/dy as a function of the scalar t
!          and the vector y.  It is to have the form
!               SUBROUTINE JAC (NEQ, T, Y, ML, MU, PD, NROWPD)
!               DIMENSION Y(*), PD(NROWPD,*)
!          where NEQ, T, Y, ML, MU, and NROWPD are input and the array
!          PD is to be loaded with partial derivatives (elements of
!          the Jacobian matrix) on output.  PD must be given a first
!          dimension of NROWPD.  T and Y have the same meaning as in
!          Subroutine F.
!               In the full matrix case (JT = 1), ML and MU are
!          ignored, and the Jacobian is to be loaded into PD in
!          columnwise manner, with df(i)/dy(j) loaded into PD(i,j).
!               In the band matrix case (JT = 4), the elements
!          within the band are to be loaded into PD in columnwise
!          manner, with diagonal lines of df/dy loaded into the rows
!          of PD.  Thus df(i)/dy(j) is to be loaded into PD(i-j+MU+1,j).
!          ML and MU are the half-bandwidth parameters (see IWORK).
!          The locations in PD in the two triangular areas which
!          correspond to nonexistent matrix elements can be ignored
!          or loaded arbitrarily, as they are overwritten by SLSODA.
!               JAC need not provide df/dy exactly.  A crude
!          approximation (possibly with a smaller bandwidth) will do.
!               In either case, PD is preset to zero by the solver,
!          so that only the nonzero elements need be loaded by JAC.
!          Each call to JAC is preceded by a call to F with the same
!          arguments NEQ, T, and Y.  Thus to gain some efficiency,
!          intermediate quantities shared by both calculations may be
!          saved in a user Common block by F and not recomputed by JAC,
!          if desired.  Also, JAC may alter the Y array, if desired.
!          JAC must be declared External in the calling program.
!               Subroutine JAC may access user-defined quantities in
!          NEQ(2),... and/or in Y(NEQ(1)+1),... if NEQ is an array
!          (dimensioned in JAC) and/or Y has length exceeding NEQ(1).
!          See the descriptions of NEQ and Y above.
!
! JT     = Jacobian type indicator.  Used only for input.
!          JT specifies how the Jacobian matrix df/dy will be
!          treated, if and when SLSODA requires this matrix.
!          JT has the following values and meanings:
!           1 means a user-supplied full (NEQ by NEQ) Jacobian.
!           2 means an internally generated (difference quotient) full
!             Jacobian (using NEQ extra calls to F per df/dy value).
!           4 means a user-supplied banded Jacobian.
!           5 means an internally generated banded Jacobian (using
!             ML+MU+1 extra calls to F per df/dy evaluation).
!          If JT = 1 or 4, the user must supply a Subroutine JAC
!          (the name is arbitrary) as described above under JAC.
!          If JT = 2 or 5, a dummy argument can be used.
!-----------------------------------------------------------------------
! Optional Inputs.
!
! The following is a list of the optional inputs provided for in the
! call sequence.  (See also Part 2.)  For each such input variable,
! this table lists its name as used in this documentation, its
! location in the call sequence, its meaning, and the default value.
! The use of any of these inputs requires IOPT = 1, and in that
! case all of these inputs are examined.  A value of zero for any
! of these optional inputs will cause the default value to be used.
! Thus to use a subset of the optional inputs, simply preload
! locations 5 to 10 in RWORK and IWORK to 0.0 and 0 respectively, and
! then set those of interest to nonzero values.
!
! Name    Location      Meaning and Default Value
!
! H0      RWORK(5)  the step size to be attempted on the first step.
!                   The default value is determined by the solver.
!
! HMAX    RWORK(6)  the maximum absolute step size allowed.
!                   The default value is infinite.
!
! HMIN    RWORK(7)  the minimum absolute step size allowed.
!                   The default value is 0.  (This lower bound is not
!                   enforced on the final step before reaching TCRIT
!                   when ITASK = 4 or 5.)
!
! IXPR    IWORK(5)  flag to generate extra printing at method switches.
!                   IXPR = 0 means no extra printing (the default).
!                   IXPR = 1 means print data on each switch.
!                   T, H, and NST will be printed on the same logical
!                   unit as used for error messages.
!
! MXSTEP  IWORK(6)  maximum number of (internally defined) steps
!                   allowed during one call to the solver.
!                   The default value is 500.
!
! MXHNIL  IWORK(7)  maximum number of messages printed (per problem)
!                   warning that T + H = T on a step (H = step size).
!                   This must be positive to result in a non-default
!                   value.  The default value is 10.
!
! MXORDN  IWORK(8)  the maximum order to be allowed for the nonstiff
!                   (Adams) method.  the default value is 12.
!                   if MXORDN exceeds the default value, it will
!                   be reduced to the default value.
!                   MXORDN is held constant during the problem.
!
! MXORDS  IWORK(9)  the maximum order to be allowed for the stiff
!                   (BDF) method.  The default value is 5.
!                   If MXORDS exceeds the default value, it will
!                   be reduced to the default value.
!                   MXORDS is held constant during the problem.
!-----------------------------------------------------------------------
! Optional Outputs.
!
! As optional additional output from SLSODA, the variables listed
! below are quantities related to the performance of SLSODA
! which are available to the user.  These are communicated by way of
! the work arrays, but also have internal mnemonic names as shown.
! except where stated otherwise, all of these outputs are defined
! on any successful return from SLSODA, and on any return with
! ISTATE = -1, -2, -4, -5, or -6.  On an illegal input return
! (ISTATE = -3), they will be unchanged from their existing values
! (if any), except possibly for TOLSF, LENRW, and LENIW.
! On any error return, outputs relevant to the error will be defined,
! as noted below.
!
! Name    Location      Meaning
!
! HU      RWORK(11) the step size in t last used (successfully).
!
! HCUR    RWORK(12) the step size to be attempted on the next step.
!
! TCUR    RWORK(13) the current value of the independent variable
!                   which the solver has actually reached, i.e. the
!                   current internal mesh point in t.  On output, TCUR
!                   will always be at least as far as the argument
!                   T, but may be farther (if interpolation was done).
!
! TOLSF   RWORK(14) a tolerance scale factor, greater than 1.0,
!                   computed when a request for too much accuracy was
!                   detected (ISTATE = -3 if detected at the start of
!                   the problem, ISTATE = -2 otherwise).  If ITOL is
!                   left unaltered but RTOL and ATOL are uniformly
!                   scaled up by a factor of TOLSF for the next call,
!                   then the solver is deemed likely to succeed.
!                   (The user may also ignore TOLSF and alter the
!                   tolerance parameters in any other way appropriate.)
!
! TSW     RWORK(15) the value of t at the time of the last method
!                   switch, if any.
!
! NST     IWORK(11) the number of steps taken for the problem so far.
!
! NFE     IWORK(12) the number of f evaluations for the problem so far.
!
! NJE     IWORK(13) the number of Jacobian evaluations (and of matrix
!                   LU decompositions) for the problem so far.
!
! NQU     IWORK(14) the method order last used (successfully).
!
! NQCUR   IWORK(15) the order to be attempted on the next step.
!
! IMXER   IWORK(16) the index of the component of largest magnitude in
!                   the weighted local error vector ( E(i)/EWT(i) ),
!                   on an error return with ISTATE = -4 or -5.
!
! LENRW   IWORK(17) the length of RWORK actually required, assuming
!                   that the length of RWORK is to be fixed for the
!                   rest of the problem, and that switching may occur.
!                   This is defined on normal returns and on an illegal
!                   input return for insufficient storage.
!
! LENIW   IWORK(18) the length of IWORK actually required, assuming
!                   that the length of IWORK is to be fixed for the
!                   rest of the problem, and that switching may occur.
!                   This is defined on normal returns and on an illegal
!                   input return for insufficient storage.
!
! MUSED   IWORK(19) the method indicator for the last successful step:
!                   1 means Adams (nonstiff), 2 means BDF (stiff).
!
! MCUR    IWORK(20) the current method indicator:
!                   1 means Adams (nonstiff), 2 means BDF (stiff).
!                   This is the method to be attempted
!                   on the next step.  Thus it differs from MUSED
!                   only if a method switch has just been made.
!
! The following two arrays are segments of the RWORK array which
! may also be of interest to the user as optional outputs.
! For each array, the table below gives its internal name,
! its base address in RWORK, and its description.
!
! Name    Base Address      Description
!
! YH      21             the Nordsieck history array, of size NYH by
!                        (NQCUR + 1), where NYH is the initial value
!                        of NEQ.  For j = 0,1,...,NQCUR, column j+1
!                        of YH contains HCUR**j/factorial(j) times
!                        the j-th derivative of the interpolating
!                        polynomial currently representing the solution,
!                        evaluated at T = TCUR.
!
! ACOR     LACOR         array of size NEQ used for the accumulated
!         (from Common   corrections on each step, scaled on output
!           as noted)    to represent the estimated local error in y
!                        on the last step.  This is the vector E in
!                        the description of the error control.  It is
!                        defined only on a successful return from
!                        SLSODA.  The base address LACOR is obtained by
!                        including in the user's program the
!                        following 2 lines:
!                           COMMON /SLS001/ RLS(218), ILS(37)
!                           LACOR = ILS(4)
!
!-----------------------------------------------------------------------
! Part 2.  Other Routines Callable.
!
! The following are optional calls which the user may make to
! gain additional capabilities in conjunction with SLSODA.
! (The routines XSETUN and XSETF are designed to conform to the
! SLATEC error handling package.)
!
!     Form of Call                  Function
!   CALL XSETUN(LUN)          set the logical unit number, LUN, for
!                             output of messages from SLSODA, if
!                             the default is not desired.
!                             The default value of LUN is 6.
!
!   CALL XSETF(MFLAG)         set a flag to control the printing of
!                             messages by SLSODA.
!                             MFLAG = 0 means do not print. (Danger:
!                             This risks losing valuable information.)
!                             MFLAG = 1 means print (the default).
!
!                             Either of the above calls may be made at
!                             any time and will take effect immediately.
!
!   CALL SSRCMA(RSAV,ISAV,JOB) saves and restores the contents of
!                             the internal Common blocks used by
!                             SLSODA (see Part 3 below).
!                             RSAV must be a real array of length 10
!                             or more, and ISAV must be an integer
!                             array of length 29 or more.
!                             JOB=1 means save Common into RSAV/ISAV.
!                             JOB=2 means restore Common from RSAV/ISAV.
!                                SSRCMA is useful if one is
!                             interrupting a run and restarting
!                             later, or alternating between two or
!                             more problems solved with SLSODA.
!
!   CALL SINTDY(,,,,,)        provide derivatives of y, of various
!        (see below)          orders, at a specified point t, if
!                             desired.  It may be called only after
!                             a successful return from SLSODA.
!
! The detailed instructions for using SINTDY are as follows.
! The form of the call is:
!
!   CALL SINTDY (T, K, RWORK(21), NYH, DKY, IFLAG)
!
! The input parameters are:
!
! T         = value of independent variable where answers are desired
!             (normally the same as the T last returned by SLSODA).
!             For valid results, T must lie between TCUR - HU and TCUR.
!             (See optional outputs for TCUR and HU.)
! K         = integer order of the derivative desired.  K must satisfy
!             0 .le. K .le. NQCUR, where NQCUR is the current order
!             (see optional outputs).  The capability corresponding
!             to K = 0, i.e. computing y(T), is already provided
!             by SLSODA directly.  Since NQCUR .ge. 1, the first
!             derivative dy/dt is always available with SINTDY.
! RWORK(21) = the base address of the history array YH.
! NYH       = column length of YH, equal to the initial value of NEQ.
!
! The output parameters are:
!
! DKY       = a real array of length NEQ containing the computed value
!             of the K-th derivative of y(t).
! IFLAG     = integer flag, returned as 0 if K and T were legal,
!             -1 if K was illegal, and -2 if T was illegal.
!             On an error return, a message is also written.
!-----------------------------------------------------------------------
! Part 3.  Common Blocks.
!
! If SLSODA is to be used in an overlay situation, the user
! must declare, in the primary overlay, the variables in:
!   (1) the call sequence to SLSODA, and
!   (2) the two internal Common blocks
!         /SLS001/  of length  34  (9 single precision words
!                      followed by 25 integer words),
!         /SLSA01/  of length   5  (1 single precision word
!                      followed by  4 integer words).
!
! If SLSODA is used on a system in which the contents of internal
! Common blocks are not preserved between calls, the user should
! declare the above Common blocks in the calling program to insure
! that their contents are preserved.
!
! If the solution of a given problem by SLSODA is to be interrupted
! and then later continued, such as when restarting an interrupted run
! or alternating between two or more problems, the user should save,
! following the return from the last SLSODA call prior to the
! interruption, the contents of the call sequence variables and the
! internal Common blocks, and later restore these values before the
! next SLSODA call for that problem.  To save and restore the Common
! blocks, use Subroutine SSRCMA (see Part 2 above).
!
!-----------------------------------------------------------------------
! Part 4.  Optionally Replaceable Solver Routines.
!
! Below is a description of a routine in the SLSODA package which
! relates to the measurement of errors, and can be
! replaced by a user-supplied version, if desired.  However, since such
! a replacement may have a major impact on performance, it should be
! done only when absolutely necessary, and only with great caution.
! (Note: The means by which the package version of a routine is
! superseded by the user's version may be system-dependent.)
!
! (a) SEWSET.
! The following subroutine is called just before each internal
! integration step, and sets the array of error weights, EWT, as
! described under ITOL/RTOL/ATOL above:
!     Subroutine SEWSET (NEQ, ITOL, RTOL, ATOL, YCUR, EWT)
! where NEQ, ITOL, RTOL, and ATOL are as in the SLSODA call sequence,
! YCUR contains the current dependent variable vector, and
! EWT is the array of weights set by SEWSET.
!
! If the user supplies this subroutine, it must return in EWT(i)
! (i = 1,...,NEQ) a positive quantity suitable for comparing errors
! in y(i) to.  The EWT array returned by SEWSET is passed to the
! SMNORM routine, and also used by SLSODA in the computation
! of the optional output IMXER, and the increments for difference
! quotient Jacobians.
!
! In the user-supplied version of SEWSET, it may be desirable to use
! the current values of derivatives of y.  Derivatives up to order NQ
! are available from the history array YH, described above under
! optional outputs.  In SEWSET, YH is identical to the YCUR array,
! extended to NQ + 1 columns with a column length of NYH and scale
! factors of H**j/factorial(j).  On the first call for the problem,
! given by NST = 0, NQ is 1 and H is temporarily set to 1.0.
! NYH is the initial value of NEQ.  The quantities NQ, H, and NST
! can be obtained by including in SEWSET the statements:
!     REAL RLS
!     COMMON /SLS001/ RLS(9),ILS(25)
!     NQ = ILS(21)
!     NST = ILS(22)
!     H = RLS(3)
! Thus, for example, the current value of dy/dt can be obtained as
! YCUR(NYH+i)/H  (i=1,...,NEQ)  (and the division by H is
! unnecessary when NST = 0).
!-----------------------------------------------------------------------
!
!***REVISION HISTORY  (YYYYMMDD)
! 19811102  DATE WRITTEN
! 19820126  Fixed bug in tests of work space lengths;
!           minor corrections in main prologue and comments.
! 19870330  Major update: corrected comments throughout;
!           removed TRET from Common; rewrote EWSET with 4 loops;
!           fixed t test in INTDY; added Cray directives in STODA;
!           in STODA, fixed DELP init. and logic around PJAC call;
!           combined routines to save/restore Common;
!           passed LEVEL = 0 in error message calls (except run abort).
! 19970225  Fixed lines setting JSTART = -2 in Subroutine LSODA.
! 20010425  Major update: convert source lines to upper case;
!           added *DECK lines; changed from 1 to * in dummy dimensions;
!           changed names R1MACH/D1MACH to RUMACH/DUMACH;
!           renamed routines for uniqueness across single/double prec.;
!           converted intrinsic names to generic form;
!           removed ILLIN and NTREP (data loaded) from Common;
!           removed all 'own' variables from Common;
!           changed error messages to quoted strings;
!           replaced XERRWV with 1993 revised version;
!           converted prologues, comments, error messages to mixed case;
!           numerous corrections to prologues and internal comments.
! 20010613  Revised excess accuracy test (to match rest of ODEPACK).
!
!-----------------------------------------------------------------------
! Other routines in the SLSODA package.
!
! In addition to Subroutine SLSODA, the SLSODA package includes the
! following subroutines and function routines:
!  SINTDY   computes an interpolated value of the y vector at t = TOUT.
!  SSTODA   is the core integrator, which does one step of the
!           integration and the associated error control.
!  SCFODE   sets all method coefficients and test constants.
!  SPRJA    computes and preprocesses the Jacobian matrix J = df/dy
!           and the Newton iteration matrix P = I - h*l0*J.
!  SSOLSY   manages solution of linear system in chord iteration.
!  SEWSET   sets the error weight vector EWT before each step.
!  SMNORM   computes the weighted max-norm of a vector.
!  SFNORM   computes the norm of a full matrix consistent with the
!           weighted max-norm on vectors.
!  SBNORM   computes the norm of a band matrix consistent with the
!           weighted max-norm on vectors.
!  SSRCMA   is a user-callable routine to save and restore
!           the contents of the internal Common blocks.
!  SGEFA and SGESL   are routines from LINPACK for solving full
!           systems of linear algebraic equations.
!  SGBFA and SGBSL   are routines from LINPACK for solving banded
!           linear systems.
!  RUMACH   computes the unit roundoff in a machine-independent manner.
!  XERRWV, XSETUN, XSETF, IXSAV, and IUMACH  handle the printing of all
!           error messages and warnings.  XERRWV is machine-dependent.
! Note:  SMNORM, SFNORM, SBNORM, RUMACH, IXSAV, and IUMACH are
! function routines.  All the others are subroutines.
!
!-----------------------------------------------------------------------
      external sprja, ssolsy
      real rumach, smnorm
!      integer icf, ierpj, iersl, jcur, jstart, kflag, l
!      integer   lyh, lewt, lacor, lsavf, lwm, liwm, meth, miter
!      integer   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
!      integer jtyp, mused, mxordn, mxords
!      integer i, i1, i2, iflag, imxer, insufr, insufi, ixpr, kgo, lf0
      integer i, i1, i2, iflag, imxer, kgo, lf0
      integer   leniw, lenrw, lenwm, ml, mord, mu, mxhnl0, mxstp0
!      integer init, mxstep, mxhnil, nhnil, nslast, nyh
      integer len1, len1c, len1n, len1s, len2, leniwc, lenrwc
!      real ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround
!      real pdnorm
      real atoli, ayi, big, ewti, h0, hmax, hmx, rh, rtoli
!      real   tcrit, tdist, tnext, tol, tolsf, tp, tsw, size, sum, w0
      real   tcrit, tdist, tnext, tol, tolsf, tp, size, sum, w0
      dimension mord(2)
      logical ihit
      character*60 msg
!      save init, mxstep, mxhnil, nhnil, nslast, nyh
!      save tsw, insufr, insufi, ixpr
!-----------------------------------------------------------------------
! the following two internal common blocks contain variables which are
! communicated between subroutines.  in each block, all real variables
! are listed first, followed by all integers.  /sls001/ is declared in
! subroutines slsoda, sintdy, sstoda, sprja, ssolsy.
! /slsa01/ is declared in subroutines slsoda, sstoda, sprja.
!-----------------------------------------------------------------------
!      common /sls001/ ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround,    &
!     &   icf, ierpj, iersl, jcur, jstart, kflag, l,                     &
!     &   lyh, lewt, lacor, lsavf, lwm, liwm, meth, miter,               &
!     &   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
!
!      common /slsa01/ pdnorm, jtyp, mused, mxordn, mxords
!
      data mord(1),mord(2)/12,5/, mxstp0/500/, mxhnl0/10/
!-----------------------------------------------------------------------
! block a.
! this code block is executed on every call.
! it tests istate and itask for legality and branches appropriately.
! if istate .gt. 1 but the flag init shows that initialization has
! not yet been done, an error return occurs.
! if istate = 1 and tout = t, return immediately.
!-----------------------------------------------------------------------
      if (istate .lt. 1 .or. istate .gt. 3) go to 601
      if (itask .lt. 1 .or. itask .gt. 5) go to 602
      if (istate .eq. 1) go to 10
      if (sloc(isr)%init .eq. 0) go to 603
      if (istate .eq. 2) go to 200
      go to 20
 10   sloc(isr)%init = 0
      if (tout .eq. t) return
!-----------------------------------------------------------------------
! block b.
! the next code block is executed for the initial call (istate = 1),
! or for a continuation call with parameter changes (istate = 3).
! it contains checking of all inputs and various initializations.
!
! first check legality of the non-optional inputs neq, itol, iopt,
! jt, ml, and mu.
!-----------------------------------------------------------------------
 20   if (neq(1) .le. 0) go to 604
      if (istate .eq. 1) go to 25
      if (neq(1) .gt. sls1(isr)%n) go to 605
 25   sls1(isr)%n = neq(1)
      if (itol .lt. 1 .or. itol .gt. 4) go to 606
      if (iopt .lt. 0 .or. iopt .gt. 1) go to 607
      if (jt .eq. 3 .or. jt .lt. 1 .or. jt .gt. 5) go to 608
      slsa(isr)%jtyp = jt
      if (jt .le. 2) go to 30
      ml = iwork(1)
      mu = iwork(2)
      if (ml .lt. 0 .or. ml .ge. sls1(isr)%n) go to 609
      if (mu .lt. 0 .or. mu .ge. sls1(isr)%n) go to 610
 30   continue
! next process and check the optional inputs. --------------------------
      if (iopt .eq. 1) go to 40
      sloc(isr)%ixpr = 0
      sloc(isr)%mxstep = mxstp0
      sloc(isr)%mxhnil = mxhnl0
      sls1(isr)%hmxi = 0.0e0
      sls1(isr)%hmin = 0.0e0
      if (istate .ne. 1) go to 60
      h0 = 0.0e0
      slsa(isr)%mxordn = mord(1)
      slsa(isr)%mxords = mord(2)
      go to 60
 40   sloc(isr)%ixpr = iwork(5)
      if (sloc(isr)%ixpr .lt. 0 .or. sloc(isr)%ixpr .gt. 1) go to 611
      sloc(isr)%mxstep = iwork(6)
      if (sloc(isr)%mxstep .lt. 0) go to 612
      if (sloc(isr)%mxstep .eq. 0) sloc(isr)%mxstep = mxstp0
      sloc(isr)%mxhnil = iwork(7)
      if (sloc(isr)%mxhnil .lt. 0) go to 613
      if (sloc(isr)%mxhnil .eq. 0) sloc(isr)%mxhnil = mxhnl0
      if (istate .ne. 1) go to 50
      h0 = rwork(5)
      slsa(isr)%mxordn = iwork(8)
      if (slsa(isr)%mxordn .lt. 0) go to 628
      if (slsa(isr)%mxordn .eq. 0) slsa(isr)%mxordn = 100
      slsa(isr)%mxordn = min(slsa(isr)%mxordn,mord(1))
      slsa(isr)%mxords = iwork(9)
      if (slsa(isr)%mxords .lt. 0) go to 629
      if (slsa(isr)%mxords .eq. 0) slsa(isr)%mxords = 100
      slsa(isr)%mxords = min(slsa(isr)%mxords,mord(2))
      if ((tout - t)*h0 .lt. 0.0e0) go to 614
 50   hmax = rwork(6)
      if (hmax .lt. 0.0e0) go to 615
      sls1(isr)%hmxi = 0.0e0
      if (hmax .gt. 0.0e0) sls1(isr)%hmxi = 1.0e0/hmax
      sls1(isr)%hmin = rwork(7)
      if (sls1(isr)%hmin .lt. 0.0e0) go to 616
!-----------------------------------------------------------------------
! set work array pointers and check lengths lrw and liw.
! if istate = 1, meth is initialized to 1 here to facilitate the
! checking of work space lengths.
! pointers to segments of rwork and iwork are named by prefixing l to
! the name of the segment.  e.g., the segment yh starts at rwork(lyh).
! segments of rwork (in order) are denoted  yh, wm, ewt, savf, acor.
! if the lengths provided are insufficient for the current method,
! an error return occurs.  this is treated as illegal input on the
! first call, but as a problem interruption with istate = -7 on a
! continuation call.  if the lengths are sufficient for the current
! method but not for both methods, a warning message is sent.
!-----------------------------------------------------------------------
 60   if (istate .eq. 1) sls1(isr)%meth = 1
      if (istate .eq. 1) sloc(isr)%nyh = sls1(isr)%n
      sls1(isr)%lyh = 21
      len1n = 20 + (slsa(isr)%mxordn + 1)*sloc(isr)%nyh
      len1s = 20 + (slsa(isr)%mxords + 1)*sloc(isr)%nyh
      sls1(isr)%lwm = len1s + 1
      if (jt .le. 2) lenwm = sls1(isr)%n*sls1(isr)%n + 2
      if (jt .ge. 4) lenwm = (2*ml + mu + 1)*sls1(isr)%n + 2
      len1s = len1s + lenwm
      len1c = len1n
      if (sls1(isr)%meth .eq. 2) len1c = len1s
      len1 = max(len1n,len1s)
      len2 = 3*sls1(isr)%n
      lenrw = len1 + len2
      lenrwc = len1c + len2
      iwork(17) = lenrw
      sls1(isr)%liwm = 1
      leniw = 20 + sls1(isr)%n
      leniwc = 20
      if (sls1(isr)%meth .eq. 2) leniwc = leniw
      iwork(18) = leniw
      if (istate .eq. 1 .and. lrw .lt. lenrwc) go to 617
      if (istate .eq. 1 .and. liw .lt. leniwc) go to 618
      if (istate .eq. 3 .and. lrw .lt. lenrwc) go to 550
      if (istate .eq. 3 .and. liw .lt. leniwc) go to 555
      sls1(isr)%lewt = len1 + 1
      sloc(isr)%insufr = 0
      if (lrw .ge. lenrw) go to 65
      sloc(isr)%insufr = 2
      sls1(isr)%lewt = len1c + 1
      msg='slsoda-  warning.. rwork length is sufficient for now, but  '
      call xerrwv (msg, 60, 103, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      msg='      may not be later.  integration will proceed anyway.   '
      call xerrwv (msg, 60, 103, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      msg = '      length needed is lenrw = i1, while lrw = i2.'
      call xerrwv (msg, 50, 103, 0, 2, lenrw, lrw, 0, 0.0e0, 0.0e0)
 65   sls1(isr)%lsavf = sls1(isr)%lewt + sls1(isr)%n
      sls1(isr)%lacor = sls1(isr)%lsavf + sls1(isr)%n
      sloc(isr)%insufi = 0
      if (liw .ge. leniw) go to 70
      sloc(isr)%insufi = 2
      msg='slsoda-  warning.. iwork length is sufficient for now, but  '
      call xerrwv (msg, 60, 104, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      msg='      may not be later.  integration will proceed anyway.   '
      call xerrwv (msg, 60, 104, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      msg = '      length needed is leniw = i1, while liw = i2.'
      call xerrwv (msg, 50, 104, 0, 2, leniw, liw, 0, 0.0e0, 0.0e0)
 70   continue
! check rtol and atol for legality. ------------------------------------
      rtoli = rtol(1)
      atoli = atol(1)
      do 75 i = 1,sls1(isr)%n
        if (itol .ge. 3) rtoli = rtol(i)
        if (itol .eq. 2 .or. itol .eq. 4) atoli = atol(i)
        if (rtoli .lt. 0.0e0) go to 619
        if (atoli .lt. 0.0e0) go to 620
 75     continue
      if (istate .eq. 1) go to 100
! if istate = 3, set flag to signal parameter changes to sstoda. -------
      sls1(isr)%jstart = -1
      if (sls1(isr)%n .eq. sloc(isr)%nyh) go to 200
! neq was reduced.  zero part of yh to avoid undefined references. -----
      i1 = sls1(isr)%lyh + sls1(isr)%l*sloc(isr)%nyh
      i2 = sls1(isr)%lyh + (sls1(isr)%maxord + 1)*sloc(isr)%nyh - 1
      if (i1 .gt. i2) go to 200
      do 95 i = i1,i2
 95     rwork(i) = 0.0e0
      go to 200
!-----------------------------------------------------------------------
! block c.
! the next block is for the initial call only (istate = 1).
! it contains all remaining initializations, the initial call to f,
! and the calculation of the initial step size.
! the error weights in ewt are inverted after being loaded.
!-----------------------------------------------------------------------
 100  sls1(isr)%uround = rumach()
      sls1(isr)%tn = t
      sloc(isr)%tsw = t
      sls1(isr)%maxord = slsa(isr)%mxordn
      if (itask .ne. 4 .and. itask .ne. 5) go to 110
      tcrit = rwork(1)
      if ((tcrit - tout)*(tout - t) .lt. 0.0e0) go to 625
      if (h0 .ne. 0.0e0 .and. (t + h0 - tcrit)*h0 .gt. 0.0e0)           &
     &   h0 = tcrit - t
 110  sls1(isr)%jstart = 0
      sloc(isr)%nhnil = 0
      sls1(isr)%nst = 0
      sls1(isr)%nje = 0
      sloc(isr)%nslast = 0
      sls1(isr)%hu = 0.0e0
      sls1(isr)%nqu = 0
      slsa(isr)%mused = 0
      sls1(isr)%miter = 0
      sls1(isr)%ccmax = 0.3e0
      sls1(isr)%maxcor = 3
      sls1(isr)%msbp = 20
      sls1(isr)%mxncf = 10
! initial call to f.  (lf0 points to yh(*,2).) -------------------------
      lf0 = sls1(isr)%lyh + sloc(isr)%nyh
      call f (isr, neq, t, y, rwork(lf0))
      sls1(isr)%nfe = 1
! load the initial value vector in yh. ---------------------------------
      do 115 i = 1,sls1(isr)%n
 115    rwork(i+sls1(isr)%lyh-1) = y(i)
! load and invert the ewt array.  (h is temporarily set to 1.0.) -------
      sls1(isr)%nq = 1
      sls1(isr)%h = 1.0e0
      call sewset (sls1(isr)%n, itol, rtol, atol, rwork(sls1(isr)%lyh), &
     &                                  rwork(sls1(isr)%lewt))
      do 120 i = 1,sls1(isr)%n
        if (rwork(i+sls1(isr)%lewt-1) .le. 0.0e0) go to 621
 120    rwork(i+sls1(isr)%lewt-1) = 1.0e0/rwork(i+sls1(isr)%lewt-1)
!-----------------------------------------------------------------------
! the coding below computes the step size, h0, to be attempted on the
! first step, unless the user has supplied a value for this.
! first check that tout - t differs significantly from zero.
! a scalar tolerance quantity tol is computed, as max(rtol(i))
! if this is positive, or max(atol(i)/abs(y(i))) otherwise, adjusted
! so as to be between 100*uround and 1.0e-3.
! then the computed value h0 is given by:
!
!   h0**(-2)  =  1./(tol * w0**2)  +  tol * (norm(f))**2
!
! where   w0     = max ( abs(t), abs(tout) ),
!         f      = the initial value of the vector f(t,y), and
!         norm() = the weighted vector norm used throughout, given by
!                  the smnorm function routine, and weighted by the
!                  tolerances initially loaded into the ewt array.
! the sign of h0 is inferred from the initial values of tout and t.
! abs(h0) is made .le. abs(tout-t) in any case.
!-----------------------------------------------------------------------
      if (h0 .ne. 0.0e0) go to 180
      tdist = abs(tout - t)
      w0 = max(abs(t),abs(tout))
      if (tdist .lt. 2.0e0*sls1(isr)%uround*w0) go to 622
      tol = rtol(1)
      if (itol .le. 2) go to 140
      do 130 i = 1,sls1(isr)%n
 130    tol = max(tol,rtol(i))
 140  if (tol .gt. 0.0e0) go to 160
      atoli = atol(1)
      do 150 i = 1,sls1(isr)%n
        if (itol .eq. 2 .or. itol .eq. 4) atoli = atol(i)
        ayi = abs(y(i))
        if (ayi .ne. 0.0e0) tol = max(tol,atoli/ayi)
 150    continue
 160  tol = max(tol,100.0e0*sls1(isr)%uround)
      tol = min(tol,0.001e0)
      sum = smnorm (sls1(isr)%n, rwork(lf0), rwork(sls1(isr)%lewt))
      sum = 1.0e0/(tol*w0*w0) + tol*sum**2
      h0 = 1.0e0/sqrt(sum)
      h0 = min(h0,tdist)
      h0 = sign(h0,tout-t)
! adjust h0 if necessary to meet hmax bound. ---------------------------
 180  rh = abs(h0)*sls1(isr)%hmxi
      if (rh .gt. 1.0e0) h0 = h0/rh
! load h with h0 and scale yh(*,2) by h0. ------------------------------
      sls1(isr)%h = h0
      do 190 i = 1,sls1(isr)%n
 190    rwork(i+lf0-1) = h0*rwork(i+lf0-1)
      go to 270
!-----------------------------------------------------------------------
! block d.
! the next code block is for continuation calls only (istate = 2 or 3)
! and is to check stop conditions before taking a step.
!-----------------------------------------------------------------------
 200  sloc(isr)%nslast = sls1(isr)%nst
      go to (210, 250, 220, 230, 240), itask
 210  if ((sls1(isr)%tn - tout)*sls1(isr)%h .lt. 0.0e0) go to 250
      call sintdy (isr, tout, 0, rwork(sls1(isr)%lyh), sloc(isr)%nyh,   &
     &             y, iflag)
      if (iflag .ne. 0) go to 627
      t = tout
      go to 420
 220  tp = sls1(isr)%tn - sls1(isr)%hu*(1.0e0+100.0e0*sls1(isr)%uround)
      if ((tp - tout)*sls1(isr)%h .gt. 0.0e0) go to 623
      if ((sls1(isr)%tn - tout)*sls1(isr)%h .lt. 0.0e0) go to 250
      t = sls1(isr)%tn
      go to 400
 230  tcrit = rwork(1)
      if ((sls1(isr)%tn - tcrit)*sls1(isr)%h .gt. 0.0e0) go to 624
      if ((tcrit - tout)*sls1(isr)%h .lt. 0.0e0) go to 625
      if ((sls1(isr)%tn - tout)*sls1(isr)%h .lt. 0.0e0) go to 245
      call sintdy (isr, tout, 0, rwork(sls1(isr)%lyh), sloc(isr)%nyh,   &
     &             y, iflag)
      if (iflag .ne. 0) go to 627
      t = tout
      go to 420
 240  tcrit = rwork(1)
      if ((sls1(isr)%tn - tcrit)*sls1(isr)%h .gt. 0.0e0) go to 624
 245  hmx = abs(sls1(isr)%tn) + abs(sls1(isr)%h)
      ihit = abs(sls1(isr)%tn - tcrit) .le. 100.0e0*sls1(isr)%uround*hmx
      if (ihit) t = tcrit
      if (ihit) go to 400
      tnext =sls1(isr)%tn + sls1(isr)%h*(1.0e0 + 4.0e0*sls1(isr)%uround)
      if ((tnext - tcrit)*sls1(isr)%h .le. 0.0e0) go to 250
      sls1(isr)%h =(tcrit - sls1(isr)%tn)*(1.0e0-4.0e0*sls1(isr)%uround)
      if (istate.eq.2 .and. sls1(isr)%jstart.ge.0) sls1(isr)%jstart = -2
!-----------------------------------------------------------------------
! block e.
! the next block is normally executed for all calls and contains
! the call to the one-step core integrator sstoda.
!
! this is a looping point for the integration steps.
!
! first check for too many steps being taken, update ewt (if not at
! start of problem), check for too much accuracy being requested, and
! check for h below the roundoff level in t.
!-----------------------------------------------------------------------
 250  continue
      if (sls1(isr)%meth .eq. slsa(isr)%mused) go to 255
      if (sloc(isr)%insufr .eq. 1) go to 550
      if (sloc(isr)%insufi .eq. 1) go to 555
 255  if ((sls1(isr)%nst-sloc(isr)%nslast).ge.sloc(isr)%mxstep)go to 500
      call sewset (sls1(isr)%n, itol, rtol, atol, rwork(sls1(isr)%lyh), &
     &                                  rwork(sls1(isr)%lewt))
      do 260 i = 1,sls1(isr)%n
        if (rwork(i+sls1(isr)%lewt-1) .le. 0.0e0) go to 510
 260    rwork(i+sls1(isr)%lewt-1) = 1.0e0/rwork(i+sls1(isr)%lewt-1)
 270  tolsf = sls1(isr)%uround                                          &
     &      * smnorm (sls1(isr)%n, rwork(sls1(isr)%lyh),                &
     &      rwork(sls1(isr)%lewt))
      if (tolsf .le. 1.0e0) go to 280
      tolsf = tolsf*2.0e0
      if (sls1(isr)%nst .eq. 0) go to 626
      go to 520
 280  if ((sls1(isr)%tn + sls1(isr)%h) .ne. sls1(isr)%tn) go to 290
      sloc(isr)%nhnil = sloc(isr)%nhnil + 1
      if (sloc(isr)%nhnil .gt. sloc(isr)%mxhnil) go to 290
      msg = 'slsoda-  warning..internal t (=r1) and h (=r2) are'
      call xerrwv (msg, 50, 101, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      msg='      such that in the machine, t + h = t on the next step  '
      call xerrwv (msg, 60, 101, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      msg = '     (h = step size). solver will continue anyway.'
      call xerrwv(msg, 50, 101, 0, 0, 0, 0, 2,sls1(isr)%tn, sls1(isr)%h)
      if (sloc(isr)%nhnil .lt. sloc(isr)%mxhnil) go to 290
      msg = 'slsoda-  above warning has been issued i1 times.  '
      call xerrwv (msg, 50, 102, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      msg = '     it will not be issued again for this problem.'
      call xerrwv (msg, 50,102, 0, 1,sloc(isr)%mxhnil, 0, 0,0.0e0,0.0e0)
 290  continue
!-----------------------------------------------------------------------
!   call sstoda(neq,y,yh,nyh,yh,ewt,savf,acor,wm,iwm,f,jac,sprja,ssolsy)
!-----------------------------------------------------------------------
      call sstoda(isr, neq, y, rwork(sls1(isr)%lyh), sloc(isr)%nyh,     &
     &   rwork(sls1(isr)%lyh),                                          &
     &   rwork(sls1(isr)%lewt),                                         &
     &   rwork(sls1(isr)%lsavf), rwork(sls1(isr)%lacor),                &
     &   rwork(sls1(isr)%lwm),                                          &
     &   iwork(sls1(isr)%liwm),                                         &
     &   f, jac, sprja, ssolsy)
      kgo = 1 - sls1(isr)%kflag
      go to (300, 530, 540), kgo
!-----------------------------------------------------------------------
! block f.
! the following block handles the case of a successful return from the
! core integrator (kflag = 0).
! if a method switch was just made, record tsw, reset maxord,
! set jstart to -1 to signal sstoda to complete the switch,
! and do extra printing of data if ixpr = 1.
! then, in any case, check for stop conditions.
!-----------------------------------------------------------------------
 300  sloc(isr)%init = 1
      if (sls1(isr)%meth .eq. slsa(isr)%mused) go to 310
      sloc(isr)%tsw = sls1(isr)%tn
      sls1(isr)%maxord = slsa(isr)%mxordn
      if (sls1(isr)%meth .eq. 2) sls1(isr)%maxord = slsa(isr)%mxords
      if (sls1(isr)%meth .eq. 2)                                        &
     &                     rwork(sls1(isr)%lwm) = sqrt(sls1(isr)%uround)
      sloc(isr)%insufr = min(sloc(isr)%insufr,1)
      sloc(isr)%insufi = min(sloc(isr)%insufi,1)
      sls1(isr)%jstart = -1
      if (sloc(isr)%ixpr .eq. 0) go to 310
      if (sls1(isr)%meth .eq. 2) then
      msg='slsoda- a switch to the bdf (stiff) method has occurred     '
      call xerrwv (msg, 60, 105, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      endif
      if (sls1(isr)%meth .eq. 1) then
      msg='slsoda- a switch to the adams (nonstiff) method has occurred'
      call xerrwv (msg, 60, 106, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      endif
      msg='     at t = r1,  tentative step size h = r2,  step nst = i1 '
      call xerrwv(msg, 60, 107, 0, 1,sls1(isr)%nst, 0, 2, sls1(isr)%tn, &
     &     sls1(isr)%h)
 310  go to (320, 400, 330, 340, 350), itask
! itask = 1.  if tout has been reached, interpolate. -------------------
 320  if ((sls1(isr)%tn - tout)*sls1(isr)%h .lt. 0.0e0) go to 250
      call sintdy (isr, tout, 0, rwork(sls1(isr)%lyh), sloc(isr)%nyh,   &
     &             y, iflag)
      t = tout
      go to 420
! itask = 3.  jump to exit if tout was reached. ------------------------
 330  if ((sls1(isr)%tn - tout)*sls1(isr)%h .ge. 0.0e0) go to 400
      go to 250
! itask = 4.  see if tout or tcrit was reached.  adjust h if necessary.
 340  if ((sls1(isr)%tn - tout)*sls1(isr)%h .lt. 0.0e0) go to 345
      call sintdy (isr, tout, 0, rwork(sls1(isr)%lyh), sloc(isr)%nyh,   &
     &             y, iflag)
      t = tout
      go to 420
 345  hmx = abs(sls1(isr)%tn) + abs(sls1(isr)%h)
      ihit = abs(sls1(isr)%tn - tcrit) .le. 100.0e0*sls1(isr)%uround*hmx
      if (ihit) go to 400
      tnext =sls1(isr)%tn + sls1(isr)%h*(1.0e0 + 4.0e0*sls1(isr)%uround)
      if ((tnext - tcrit)*sls1(isr)%h .le. 0.0e0) go to 250
      sls1(isr)%h =(tcrit - sls1(isr)%tn)*(1.0e0-4.0e0*sls1(isr)%uround)
      if (sls1(isr)%jstart .ge. 0) sls1(isr)%jstart = -2
      go to 250
! itask = 5.  see if tcrit was reached and jump to exit. ---------------
 350  hmx = abs(sls1(isr)%tn) + abs(sls1(isr)%h)
      ihit = abs(sls1(isr)%tn - tcrit) .le. 100.0e0*sls1(isr)%uround*hmx
!-----------------------------------------------------------------------
! block g.
! the following block handles all successful returns from slsoda.
! if itask .ne. 1, y is loaded from yh and t is set accordingly.
! istate is set to 2, and the optional outputs are loaded into the
! work arrays before returning.
!-----------------------------------------------------------------------
 400  do 410 i = 1,sls1(isr)%n
 410    y(i) = rwork(i+sls1(isr)%lyh-1)
      t = sls1(isr)%tn
      if (itask .ne. 4 .and. itask .ne. 5) go to 420
      if (ihit) t = tcrit
 420  istate = 2
      rwork(11) = sls1(isr)%hu
      rwork(12) = sls1(isr)%h
      rwork(13) = sls1(isr)%tn
      rwork(15) = sloc(isr)%tsw
      iwork(11) = sls1(isr)%nst
      iwork(12) = sls1(isr)%nfe
      iwork(13) = sls1(isr)%nje
      iwork(14) = sls1(isr)%nqu
      iwork(15) = sls1(isr)%nq
      iwork(19) = slsa(isr)%mused
      iwork(20) = sls1(isr)%meth
      return
!-----------------------------------------------------------------------
! block h.
! the following block handles all unsuccessful returns other than
! those for illegal input.  first the error message routine is called.
! if there was an error test or convergence test failure, imxer is set.
! then y is loaded from yh and t is set to tn.
! the optional outputs are loaded into the work arrays before returning.
!-----------------------------------------------------------------------
! the maximum number of steps was taken before reaching tout. ----------
 500  msg = 'slsoda-  at current t (=r1), mxstep (=i1) steps   '
      call xerrwv (msg, 50, 201, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      msg = '      taken on this call before reaching tout     '
      call xerrwv (msg, 50, 201, 0, 1, sloc(isr)%mxstep, 0, 1,          &
     &     sls1(isr)%tn, 0.0e0)
      istate = -1
      go to 580
! ewt(i) .le. 0.0 for some i (not at start of problem). ----------------
 510  ewti = rwork(sls1(isr)%lewt+i-1)
      msg = 'slsoda-  at t (=r1), ewt(i1) has become r2 .le. 0.'
      call xerrwv (msg, 50, 202, 0, 1, i, 0, 2, sls1(isr)%tn, ewti)
      istate = -6
      go to 580
! too much accuracy requested for machine precision. -------------------
 520  msg = 'slsoda-  at t (=r1), too much accuracy requested  '
      call xerrwv (msg, 50, 203, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      msg = '      for precision of machine..  see tolsf (=r2) '
      call xerrwv (msg, 50, 203, 0, 0, 0, 0, 2, sls1(isr)%tn, tolsf)
      rwork(14) = tolsf
      istate = -2
      go to 580
! kflag = -1.  error test failed repeatedly or with abs(h) = hmin. -----
 530  msg = 'slsoda-  at t(=r1) and step size h(=r2), the error'
      call xerrwv (msg, 50, 204, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      msg = '      test failed repeatedly or with abs(h) = hmin'
      call xerrwv (msg, 50, 204, 0, 0, 0, 0, 2,sls1(isr)%tn,sls1(isr)%h)
      istate = -4
      go to 560
! kflag = -2.  convergence failed repeatedly or with abs(h) = hmin. ----
 540  msg = 'slsoda-  at t (=r1) and step size h (=r2), the    '
      call xerrwv (msg, 50, 205, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      msg = '      corrector convergence failed repeatedly     '
      call xerrwv (msg, 50, 205, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      msg = '      or with abs(h) = hmin   '
      call xerrwv (msg, 30, 205, 0, 0, 0, 0, 2,sls1(isr)%tn,sls1(isr)%h)
      istate = -5
      go to 560
! rwork length too small to proceed. -----------------------------------
 550  msg = 'slsoda-  at current t(=r1), rwork length too small'
      call xerrwv (msg, 50, 206, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      msg='      to proceed.  the integration was otherwise successful.'
      call xerrwv (msg, 60, 206, 0, 0, 0, 0, 1, sls1(isr)%tn, 0.0e0)
      istate = -7
      go to 580
! iwork length too small to proceed. -----------------------------------
 555  msg = 'slsoda-  at current t(=r1), iwork length too small'
      call xerrwv (msg, 50, 207, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      msg='      to proceed.  the integration was otherwise successful.'
      call xerrwv (msg, 60, 207, 0, 0, 0, 0, 1, sls1(isr)%tn, 0.0e0)
      istate = -7
      go to 580
! compute imxer if relevant. -------------------------------------------
 560  big = 0.0e0
      imxer = 1
      do 570 i = 1,sls1(isr)%n
        size = abs(rwork(i+sls1(isr)%lacor-1)*rwork(i+sls1(isr)%lewt-1))
        if (big .ge. size) go to 570
        big = size
        imxer = i
 570    continue
      iwork(16) = imxer
! set y vector, t, and optional outputs. -------------------------------
 580  do 590 i = 1,sls1(isr)%n
 590    y(i) = rwork(i+sls1(isr)%lyh-1)
      t = sls1(isr)%tn
      rwork(11) = sls1(isr)%hu
      rwork(12) = sls1(isr)%h
      rwork(13) = sls1(isr)%tn
      rwork(15) = sloc(isr)%tsw
      iwork(11) = sls1(isr)%nst
      iwork(12) = sls1(isr)%nfe
      iwork(13) = sls1(isr)%nje
      iwork(14) = sls1(isr)%nqu
      iwork(15) = sls1(isr)%nq
      iwork(19) = slsa(isr)%mused
      iwork(20) = sls1(isr)%meth
      return
!-----------------------------------------------------------------------
! block i.
! the following block handles all error returns due to illegal input
! (istate = -3), as detected before calling the core integrator.
! first the error message routine is called.  if the illegal input
! is a negative istate, the run is aborted (apparent infinite loop).
!-----------------------------------------------------------------------
 601  msg = 'slsoda-  istate (=i1) illegal.'
      call xerrwv (msg, 30, 1, 0, 1, istate, 0, 0, 0.0e0, 0.0e0)
      if (istate .lt. 0) go to 800
      go to 700
 602  msg = 'slsoda-  itask (=i1) illegal. '
      call xerrwv (msg, 30, 2, 0, 1, itask, 0, 0, 0.0e0, 0.0e0)
      go to 700
 603  msg = 'slsoda-  istate .gt. 1 but slsoda not initialized.'
      call xerrwv (msg, 50, 3, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      go to 700
 604  msg = 'slsoda-  neq (=i1) .lt. 1     '
      call xerrwv (msg, 30, 4, 0, 1, neq(1), 0, 0, 0.0e0, 0.0e0)
      go to 700
 605  msg = 'slsoda-  istate = 3 and neq increased (i1 to i2). '
      call xerrwv (msg, 50, 5, 0, 2,sls1(isr)%n,neq(1), 0, 0.0e0, 0.0e0)
      go to 700
 606  msg = 'slsoda-  itol (=i1) illegal.  '
      call xerrwv (msg, 30, 6, 0, 1, itol, 0, 0, 0.0e0, 0.0e0)
      go to 700
 607  msg = 'slsoda-  iopt (=i1) illegal.  '
      call xerrwv (msg, 30, 7, 0, 1, iopt, 0, 0, 0.0e0, 0.0e0)
      go to 700
 608  msg = 'slsoda-  jt (=i1) illegal.    '
      call xerrwv (msg, 30, 8, 0, 1, jt, 0, 0, 0.0e0, 0.0e0)
      go to 700
 609  msg = 'slsoda-  ml (=i1) illegal: .lt.0 or .ge.neq (=i2) '
      call xerrwv (msg, 50, 9, 0, 2, ml, neq(1), 0, 0.0e0, 0.0e0)
      go to 700
 610  msg = 'slsoda-  mu (=i1) illegal: .lt.0 or .ge.neq (=i2) '
      call xerrwv (msg, 50, 10, 0, 2, mu, neq(1), 0, 0.0e0, 0.0e0)
      go to 700
 611  msg = 'slsoda-  ixpr (=i1) illegal.  '
      call xerrwv (msg, 30, 11, 0, 1,sloc(isr)%ixpr, 0, 0, 0.0e0, 0.0e0)
      go to 700
 612  msg = 'slsoda-  mxstep (=i1) .lt. 0  '
      call xerrwv (msg, 30, 12, 0, 1,sloc(isr)%mxstep, 0, 0,0.0e0,0.0e0)
      go to 700
 613  msg = 'slsoda-  mxhnil (=i1) .lt. 0  '
      call xerrwv (msg, 30, 13, 0, 1,sloc(isr)%mxhnil, 0, 0,0.0e0,0.0e0)
      go to 700
 614  msg = 'slsoda-  tout (=r1) behind t (=r2)      '
      call xerrwv (msg, 40, 14, 0, 0, 0, 0, 2, tout, t)
      msg = '      integration direction is given by h0 (=r1)  '
      call xerrwv (msg, 50, 14, 0, 0, 0, 0, 1, h0, 0.0e0)
      go to 700
 615  msg = 'slsoda-  hmax (=r1) .lt. 0.0  '
      call xerrwv (msg, 30, 15, 0, 0, 0, 0, 1, hmax, 0.0e0)
      go to 700
 616  msg = 'slsoda-  hmin (=r1) .lt. 0.0  '
      call xerrwv (msg, 30, 16, 0, 0, 0, 0, 1, sls1(isr)%hmin, 0.0e0)
      go to 700
 617  msg='slsoda-  rwork length needed, lenrw (=i1), exceeds lrw (=i2)'
      call xerrwv (msg, 60, 17, 0, 2, lenrw, lrw, 0, 0.0e0, 0.0e0)
      go to 700
 618  msg='slsoda-  iwork length needed, leniw (=i1), exceeds liw (=i2)'
      call xerrwv (msg, 60, 18, 0, 2, leniw, liw, 0, 0.0e0, 0.0e0)
      go to 700
 619  msg = 'slsoda-  rtol(i1) is r1 .lt. 0.0        '
      call xerrwv (msg, 40, 19, 0, 1, i, 0, 1, rtoli, 0.0e0)
      go to 700
 620  msg = 'slsoda-  atol(i1) is r1 .lt. 0.0        '
      call xerrwv (msg, 40, 20, 0, 1, i, 0, 1, atoli, 0.0e0)
      go to 700
 621  ewti = rwork(sls1(isr)%lewt+i-1)
      msg = 'slsoda-  ewt(i1) is r1 .le. 0.0         '
      call xerrwv (msg, 40, 21, 0, 1, i, 0, 1, ewti, 0.0e0)
      go to 700
 622  msg='slsoda-  tout(=r1) too close to t(=r2) to start integration.'
      call xerrwv (msg, 60, 22, 0, 0, 0, 0, 2, tout, t)
      go to 700
 623  msg='slsoda-  itask = i1 and tout (=r1) behind tcur - hu (= r2)  '
      call xerrwv (msg, 60, 23, 0, 1, itask, 0, 2, tout, tp)
      go to 700
 624  msg='slsoda-  itask = 4 or 5 and tcrit (=r1) behind tcur (=r2)   '
      call xerrwv (msg, 60, 24, 0, 0, 0, 0, 2, tcrit, sls1(isr)%tn)
      go to 700
 625  msg='slsoda-  itask = 4 or 5 and tcrit (=r1) behind tout (=r2)   '
      call xerrwv (msg, 60, 25, 0, 0, 0, 0, 2, tcrit, tout)
      go to 700
 626  msg = 'slsoda-  at start of problem, too much accuracy   '
      call xerrwv (msg, 50, 26, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      msg='      requested for precision of machine..  see tolsf (=r1) '
      call xerrwv (msg, 60, 26, 0, 0, 0, 0, 1, tolsf, 0.0e0)
      rwork(14) = tolsf
      go to 700
 627  msg = 'slsoda-  trouble in sintdy.  itask = i1, tout = r1'
      call xerrwv (msg, 50, 27, 0, 1, itask, 0, 1, tout, 0.0e0)
      go to 700
 628  msg = 'slsoda-  mxordn (=i1) .lt. 0  '
      call xerrwv (msg, 30,28, 0, 1, slsa(isr)%mxordn, 0, 0,0.0e0,0.0e0)
      go to 700
 629  msg = 'slsoda-  mxords (=i1) .lt. 0  '
      call xerrwv (msg, 30,29, 0, 1, slsa(isr)%mxords, 0, 0,0.0e0,0.0e0)
!
 700  istate = -3
      return
!
 800  msg = 'slsoda-  run aborted.. apparent infinite loop.    '
      call xerrwv (msg, 50, 303, 2, 0, 0, 0, 0, 0.0e0, 0.0e0)
      return
!----------------------- end of subroutine slsoda ----------------------
      end
