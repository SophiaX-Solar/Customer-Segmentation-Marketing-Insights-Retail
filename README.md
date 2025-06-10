# SmartFresh Customer Segmentation & Marketing Insights

This project explores customer segmentation and marketing effectiveness for **SmartFresh**, a premium food and wine retailer. Using real-world transaction-level data, I applied statistical and machine learning methods to identify key customer segments and provide strategic recommendations for personalised marketing.

<br/>

## Project Overview

Modern retailers often face challenges such as:
- Over-reliance on discount-driven sales
- Limited customer insight beyond demographics
- Ineffective marketing segmentation

This project aims to solve those issues by combining **behavioural segmentation**, **RFM analysis**, and **PCA-based clustering** to uncover:
- High-value customers who contribute most to revenue
- Low-engagement users are at risk of churn
- Price-sensitive buyers are responsive to campaigns

<br/>

## Techniques Used

| Area | Methods |
|------|---------|
| Data Cleaning & Exploration | Handling missing income values, outlier detection |
| Behavioural Modelling | RFM (Recency, Frequency, Monetary) analysis |
| Hypothesis Testing | Welch t-test with Yeo-Johnson transformation |
| Feature Engineering | Promotion responsiveness, luxury preference |
| Dimensionality Reduction | Principal Component Analysis (PCA) |
| Customer Segmentation | K-Means clustering with silhouette scoring |
| Visualisation | Radar charts, scree plots, income heatmaps |

All analyses were conducted using R.

<br/>

## Key Insights

- **Promotions drive indirect value**: Though few customers redeem offers, promotions subtly increase overall spending among non-redeemers.
- **High-value customers are brand-driven**: They spend frequently on premium goods but ignore discounts. Targeting should focus on exclusivity and loyalty experiences.
- **Low-engagement group is at risk**: A sizable segment has low activity and moderate price sensitivity. These are ideal for personalised reactivation campaigns.
- **Three clusters identified**:
  - **Cluster 1**: Price-oriented, low purchase frequency
  - **Cluster 2**: Low engagement, moderate value
  - **Cluster 3**: High spenders, premium-focused

<br/>

## Recommendations

| Segment | Suggested Strategy |
|---------|--------------------|
| **Price-sensitive** | Flash deals, limited-time bundles, loyalty points |
| **Low-engagement** | Reactivation emails, targeted remarketing |
| **High-value** | VIP programs, private tasting events, premium delivery services |

Product strategies were also adapted by category (e.g., wine vs. wellness products) using involvement theory and brand positioning logic.

<br/>

## Repository Structure

