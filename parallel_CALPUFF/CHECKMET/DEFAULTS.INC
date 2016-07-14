C****************************************************************
C     Ausplume and ISC3 Meteorological Data Checking Utility
C
C     Vincent Tino
C     Earth Tech, Inc.
C     196 Baker Ave.
C     Concord, MA 01742
C     (508) 371-4200
C
C****************************************************************

c *** DEFAULT CHECKMET VARIABLES
c --- RUNTIME OPTIONS
      cmodel='aus'
      chktype='REG'
      ibegyr=0
      ibegmo=1
      ibegdy=1
      ibeghr=1
      iendyr=99
      iendmo=12
      ienddy=31
      iendhr=24
      
      
c --- AUSPLUME OPTIONS
      ltempaus=.true.        
      lwsaus=.true.          
      lwdaus=.true.          
      lstabaus=.true.        
      lziaus=.true.          
      lsigtaus=.true.        
      lpexpaus=.false.       
      ldthdzaus=.false.      
      ldecayaus=.false.      
      lpcodeaus=.false.      
      lprateaus=.false.      
      lswradaus=.false.      
      lrhaus=.false.         
                      
c --- ISC OPTIONS
      lflowisc=.true.        
      lwsisc=.true.          
      ltempisc=.true.        
      lstabisc=.true.        
      lruralzisc=.true.      
      lurbanzisc=.true.      
      lpexpisc=.false.       
      ldthdzisc=.false.      
      lustarisc=.false.      
      lmoninisc=.false.      
      lzknotisc=.false.      
      lpcodeisc=.false.      
      lprateisc=.false.
      lswisc=.false.
      lrhisc=.false.

c --- AUSPLUME DEFAULT RANGES
      mintempaus=-40.0       
      maxtempaus=50.0        
      minwsaus=0.0           
      maxwsaus=33.5          
      minwdaus=0             
      maxwdaus=360           
      minziaus=0.0           
      maxziaus=4000.0        
      minsigaus=0.0        
      maxsigaus=100.0        
      minpaus=0.0            
      maxpaus=5.0            
      mindthdzaus=0.000      
      maxdthdzaus=2.000      
      mindecayaus=0.0        
      maxdecayaus=10.0       
      minpcaus=0             
      maxpcaus=50            
      minpraus=0.0           
      maxpraus=20.0          
      minswaus=0.0           
      maxswaus=2000.0        
      minrhaus=0.0           
      maxrhaus=100.0         

c --- ISC DEFAULT RANGES   
      minflowisc=0           
      maxflowisc=360         
      minwsisc=0.0           
      maxwsisc=33.5          
      mintempisc=270.0       
      maxtempisc=315.0        
      minruralzisc=0.0       
      maxruralzisc=4000.0    
      minurbanzisc=0.0       
      maxurbanzisc=4000.0    
      minpexpisc=0.0         
      maxpexpisc=5.0         
      mindthdzisc=0.000      
      maxdthdzisc=2.000      
      minustarisc=0.01       
      maxustarisc=0.3       
      minmoninisc=-150.0     
      maxmoninisc=200.0      
      minzknotisc=0.0001     
      maxzknotisc=1.0       
      minpcodeisc=0          
      maxpcodeisc=50         
      minprateisc=0.0        
      maxprateisc=20.0       
      minswisc=0.0           
      maxswisc=2000.0        
      minrhisc=0.0           
      maxrhisc=100.0         
                             


