# statistic_competition_2022

## 건국대학교 응용통계학과 '통계최강자전' 대회 코드
### 최종 결과 : 1위 대상 🥇

대회에서 주어지는 데이터셋

<img width="728" alt="5" src="https://user-images.githubusercontent.com/97430653/201526356-28e8f7c2-0e7c-47a7-9f7b-cbd1c764e548.png">


선정한 데이터셋 출처 : https://www.kaggle.com/datasets/arashnic/imbalanced-data-practice?select=aug_train.csv 



## 주제 :  통계적 분석을 통한 자동차 보험 판매 예측 모델링 및 군집화

### 1. introduction

- 선정 배경
    - 기업이 마케팅 활동을 전개하는 데 있어 고객들의 상품 선택 행동에 대한 이해와 예측은 매우 필수적인 요소이다.
    - 어떠한 특성을 가진 고객이 어떤 상품을 선택할 것인지를 이해하고 사전에 예측하는 능력은 고객별로 맞춤형 마케팅 프로그램을 제공하는 타깃 마케팅을 전개하는 데 있어 반드시 가져야 할 핵심적인 역량이기 때문이다.
    - 오늘날의 기업들은 이른바 빅데이터 시대라고 불리는 경영 환경을 마주하고 있는데, 특히 마케팅 분야는 이러한 방대한 데이터들이 생성되고 활용되는 최전선에 있다고 해도 과언이 아닐 정도로 다양한 고객 관련 데이터들이 생성되고 있다.
    - 이러한 흐름에 따라 우리는 보험사의 고객 데이터를 활용한 데이터 분석을 통해 고객 맞춤형 마케팅을 제안하고자 한다.
- 분석 목표
    - 기존 건강보험 가입자들 중 자동차 보험에 대해 긍정적으로 응답한 고객들에 대하여 가장 잘 예측할 수 있는 최적 모델 찾기
    - 최적 모델에서 자동차 보험에 긍정적일 것으로 예측된 회원들의 특성에 가장 중요하게 영향을 미치는 변수를 찾아, 자동차 보험 판매 프로모션 방향성의 인사이트 찾기
    - 군집분석을 통하여 군집별 특성을 확인한 후 비교함으로써 프로모션 방향성의 인사이트 찾기
    - 모델링과 군집분석을 통한 자동차 보험 프로모션 방향성 제시

### 2. Data Exploration

- 데이터 소개
    - 불필요한 변수(id)삭제
- 시각화
    - target값 불균형 분포
    - 수치형 변수들과 target값의 분포
    - 범주형 변수들과 target값의 분포
    - 상관관계 확인→ 다중공선성 의심
- 전처리
    - 결측치 처리
        - 결측치 존재 x
    - 인코딩
        - label 인코딩
    - 이상치처리
        - boxplot으로 칼럼 별 분포 확인 후 이상치 대체
    - 스케일링
        - log 변환

### 3. Modeling
- 모델 소개
    - 통계적 모델 vs 머신러닝 모델(트리 모델)
    - 통계적 모델의 특징 : 해석 가능 & 비교적 낮은 예측성능
    - 머신러닝 모델의 특징 : 복잡한 비선형관계를 충분히 학습 가능 ➔ 충분히 많은 관측치/모델 특성상 과적합 우려 적음
    & 정보량이 적은 변수에 강건함 ➔ 예측에 유의미하지 않은 변수로 인해 받는 악영향이 적음
    
- 통계적 모델
    - 로지스틱회귀 가설검정
        - logistic regression을 선택한 이유 - 통계의 해석적 측면 강조
        - 로지스틱 회귀 모델에 대한 가설 검정
        - 개별 회귀 계수에 대한 가설 검정 : vintage의 p-value가 0.05보다 작아 유의미하지 않다고 판단
        - 변수선택법(단계선택법) step : vintage 제거
        → anova에서도 vintage, 변수선택법(단계선택법)에서도 vintage이므로 vintage를 제거하는 것으로 결정
        
- 머신러닝 모델 (Tree 기반 모델)
    - RandomForest, XGBoost, CatBoost 비교 → 성능이 높았던 Catboost 선정
    - CatBoost 모델의 다중공선성 처리 전후 비교 
    - CatBoost 모델의 불균형 처리 전후 비교 : class_weight 파라미터 
    - 최종적으로 CatBoost 모델 선정


- 모델 구축
    - CatBoost 모델의 불균형 처리 전후 비교 : class_weight 파라미터 설정
    - Parameter Tuning : Bayesian Optimization
    - threshold 변경 → threshold값 변경을 통해 조금 더 보수적으로 보험 신청 여부 판단
    - SHAP value를 통한 유의미한 변수 고찰

### 4. Clustering

- 군집화를 위한 추가 전처리
    - 변수 선택
    - one-hot 인코딩
    - log 변환
    - 시각화를 위해 2차원으로 PCA
- K-means
- K-means 군집 개수 평가지표
    1. 실루엣 계수
    2. Elbow point
    ➔ 두 지표를 확인하여 k = 3일때 선정
    
    군집 시각화 결과 
    
 <img src="https://github.com/eunbinni/statistic_competition_2022/blob/main/images/4.png" width="400" height="250"/>

- 해석

### 5. Conclusion
  - 모델링
      - 이전 보험 가입여부(Previously_Insured) / 차량 손상 여부(Vehicle_Damage) / 연령대(Age_level)/ 판매 채널(Policy_sales_channel)' 변수는 자동차 보험에 대해 높은 영향을 미치는 중요한 변수로 판단
      - 특히 경제 활동 인구가 많은 취업 후 ~ 은퇴 전 연령대에서 자동차 보험에 긍정적일 확률이 높은 것으로 보아 이 연령층에 마케팅을 집중
      - 판매 채널의 경우 채널별로 편차가 큰 것으로 보이며 높은 반응을 보이는 26,124,152 채널에서 위의 연령층에 마케팅을 집중
      - 상대적으로 낮은 반응을 보이는 판매채널에서는 마케팅을 줄이는 방향으로 진행하여 손실을 최소화, 혹은 핵심 연령층을 신규로 확보하는 마케팅 전략 수립
     <img src="https://github.com/eunbinni/statistic_competition_2022/blob/main/images/%E1%84%89%E1%85%B3%E1%84%8F%E1%85%B3%E1%84%85%E1%85%B5%E1%86%AB%E1%84%89%E1%85%A3%E1%86%BA%202022-11-07%20%E1%84%8B%E1%85%A9%E1%84%92%E1%85%AE%208.30.21.png" width="530" height="300"/>
     
     
      - PDP plot 'Age_level'
      <img src="https://github.com/eunbinni/statistic_competition_2022/blob/main/images/%E1%84%89%E1%85%B3%E1%84%8F%E1%85%B3%E1%84%85%E1%85%B5%E1%86%AB%E1%84%89%E1%85%A3%E1%86%BA%202022-11-07%20%E1%84%8B%E1%85%A9%E1%84%92%E1%85%AE%208.30.32.png" width="500" height="300"/>
      
      ➔ 경제 활동 인구가 많은 취업 후 ~ 은퇴 전 연령대에서 자동차 보험에 긍정적일 확률이 높음을 확인할 수 있음


  - 군집화
      - A군집은 20대(취업 전 연령대)가 대부분이며, 응답률 매우 낮은 군집 ➔ 자동차 보험의 중요성을 알리는 방향으로 마케팅 전략 수립
      - B 군집은 40~60대(내 집 마련 연령대)가 대부분이며, 응답률이 매우 높은 군집 ➔ B 군집 중 자동차 보험 가입에 응답하지 않은 사람들을 대상으로 40대 ~ 60대의 교통사고 비율이 높다는 통계자료*를 제시하여 자동차 보험 가입을 유도하는 마케팅 전략 수립
      - C 군집은 뚜렷한 특징이 나타나지 않는 평범한 군집 ➔ 모든 사람들이 받아들일 만한 효과적인 마케팅 방안 제시


  - 한계점
      - 실제 현업에서는 판매채널별/지역별 상세 정보/고객 별 개인정보 등을 추가로 분석해 더 효과적인 마케팅 인사이트를 얻을 수 있을 것으로 판단
      - 데이터의 한계로 인해 다양한 전처리 방법과 세밀한 군집화를 해보지 못함. 더욱 다양한 정보를 가진 데이터가 있었다면 더 정밀하고 효과적인 인사이트 도출이 가능했을거라 생각
      - 군집화 진행 후 A 군집의 경우, 응답률이 낮음에도 불구하고 정확한 정보가 없었던 Previously_Insured가 왜 높은 비율로 있는지 해석할 수 없었음. 따라서 Previously_Insured의 정확한 정보가 필요할 것으로 판단
      - 전공 지식 수준의 한계로, 다중공선성과 같이 깊은 이해를 바탕으로 한 분석을 진행하지 못함. 향후 전공 수업을 통해 많은 통계적 지식을 얻은 후 더 나은 분석을 할 수 있을 것으로 기대

