import pandas as pd, numpy as np, matplotlib; matplotlib.use('Agg')
import matplotlib.pyplot as plt, seaborn as sns, statsmodels.api as sm
from scipy import stats
from sklearn.model_selection import train_test_split, KFold, cross_val_score
from sklearn.preprocessing import StandardScaler, PolynomialFeatures
from sklearn.linear_model import LinearRegression
plt.rcParams.update({'font.family':'serif','font.size':10,'axes.spines.top':False,'axes.spines.right':False,'savefig.dpi':300,'savefig.bbox':'tight'})
C1,C2='#2b5c8a','#c0504d'
df=pd.read_excel('gym_members_exercise_tracking.xlsx').drop(columns='BMI')
df['Gender']=(df.Gender=='Male').astype(int)
df=pd.get_dummies(df,columns=['Workout_Type'],drop_first=True,dtype=int)
X=df.drop(columns='Calories_Burned'); y=df.Calories_Burned
Xtr,Xte,ytr,yte=train_test_split(X,y,test_size=.2,random_state=42)
sc=StandardScaler().fit(Xtr); A,B=sc.transform(Xtr),sc.transform(Xte)
mlr=LinearRegression().fit(A,ytr); pm=mlr.predict(B)
pf=PolynomialFeatures(2,include_bias=False); PA=pf.fit_transform(A); PB=pf.transform(B)
pr=LinearRegression().fit(PA,ytr); pp=pr.predict(PB)
kf=KFold(5,shuffle=True,random_state=42)
cv=lambda M: np.sqrt(-cross_val_score(LinearRegression(),M,ytr,cv=kf,scoring='neg_mean_squared_error'))
deg=[]
for d in [1,2,3]:
    r=cv(PolynomialFeatures(d,include_bias=False).fit_transform(A)); deg.append((d,r.mean(),r.std()))
fm,fp=cv(A),cv(PA); dif=fm-fp

f,ax=plt.subplots(figsize=(6,3.8)); ax.hist(y,bins=30,color=C1,edgecolor='white'); ax.set_xlabel('Calories burned (kcal)'); ax.set_ylabel('Frequency'); f.savefig('fig/fig1_hist.png'); plt.close()
f,ax=plt.subplots(figsize=(6,3.8)); ax.scatter(df['Session_Duration (hours)'],y,s=10,alpha=.6,color=C1); ax.set_xlabel('Session duration (h)'); ax.set_ylabel('Calories burned (kcal)'); f.savefig('fig/fig2_scatter.png'); plt.close()
corr=df.corr(); lab=[c.replace('Workout_Type_','WT_').replace(' (days/week)','').replace(' (liters)','').replace(' (hours)','').replace(' (kg)','').replace(' (m)','') for c in corr.columns]
f,ax=plt.subplots(figsize=(9,7.5)); sns.heatmap(corr,annot=True,fmt='.2f',cmap='coolwarm',vmin=-1,vmax=1,xticklabels=lab,yticklabels=lab,annot_kws={'size':6.5},cbar_kws={'shrink':.8},ax=ax); f.savefig('fig/fig3_heatmap.png'); plt.close()
res=yte-pm
f,ax=plt.subplots(1,2,figsize=(9,3.6))
ax[0].scatter(pm,res,s=12,alpha=.7,color=C1); ax[0].axhline(0,color=C2,ls='--'); ax[0].set_xlabel('Predicted (kcal)'); ax[0].set_ylabel('Residual (kcal)'); ax[0].set_title('(a) Residuals vs predicted')
sm.qqplot(res,line='s',ax=ax[1],markersize=3); ax[1].set_title('(b) Normal Q–Q of residuals'); f.tight_layout(); f.savefig('fig/fig4_residuals.png'); plt.close()
f,ax=plt.subplots(figsize=(5,3.6)); dd=np.array(deg); ax.errorbar(dd[:,0],dd[:,1],yerr=dd[:,2],marker='o',color=C1,capsize=4); ax.set_xticks([1,2,3]); ax.set_xlabel('Polynomial degree'); ax.set_ylabel('Mean 5-fold CV RMSE (kcal)')
for d,m,s in deg: ax.annotate(f'{m:.2f}',(d,m),textcoords='offset points',xytext=(8,4))
f.savefig('fig/fig5_degree.png'); plt.close()
lo,hi=yte.min(),yte.max()
f,ax=plt.subplots(1,2,figsize=(9,4),sharey=True)
for a,p,t in [(ax[0],pm,'(a) MLR'),(ax[1],pp,'(b) Polynomial (degree 2)')]:
    a.scatter(yte,p,s=12,alpha=.7,color=C1); a.plot([lo,hi],[lo,hi],'--',color=C2); a.set_title(t); a.set_xlabel('Actual (kcal)')
ax[0].set_ylabel('Predicted (kcal)'); f.tight_layout(); f.savefig('fig/fig6_actual_pred.png'); plt.close()
f,ax=plt.subplots(figsize=(5.5,3.8))
for k in range(5): ax.plot([0,1],[fm[k],fp[k]],color='grey',lw=1)
ax.scatter([0]*5,fm,color=C2,zorder=3,label='MLR'); ax.scatter([1]*5,fp,color=C1,zorder=3,label='Polynomial')

ax.set_xticks([0,1]); ax.set_xticklabels(['MLR','Polynomial (degree 2)']); ax.set_xlim(-.4,1.4); ax.set_ylabel('Fold RMSE (kcal)'); f.savefig('fig/fig7_folds.png'); plt.close()
f,ax=plt.subplots(figsize=(4.5,3.8)); sm.qqplot(dif,line='s',ax=ax,markersize=5); ax.set_ylabel('Paired difference (kcal)'); f.savefig('fig/fig8_qq_diff.png'); plt.close()
print(deg,fm,fp,dif,stats.shapiro(dif),stats.shapiro(res),len(res))
