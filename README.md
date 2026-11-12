# TripAdvisor Travel Reviews: Consumer Segmentation & Recommender Strategy

Built with R (RStudio, R Markdown)

**Packages used:** `ade4`, `cluster`, `factoextra`, `corrplot`, `car`, `plotly`

<br>

## 1. Business Context & Dataset
- **Source:** UCI Machine Learning Repository (*Travel Reviews Dataset*, Renjith et al., 2018)
- **Scope:** 980 unique users across East Asian destinations
- **Features:** 10 continuous variables representing average user ratings on a `0` (Terrible) to `4` (Excellent) scale: *Art galleries, Dance clubs, Juice bars, Restaurants, Museums, Resorts, Parks/picnic spots, Beaches, Theaters, and Religious institutions*
- **Data Integrity:** Complete dataset (no missing values).

Note: `Parks` exhibits near-zero variance and was excluded prior to dimensionality reduction to prevent variance distortion during standardisation.

<br>

## 2. Methodology
1. **Exploratory Data Analysis (EDA):** Distribution profiling, variance analysis, and bivariate correlation testing (identifying high-correlation / low-slope traps such as *Parks vs. Juice bars*).
2. **Dimensionality Reduction (PCA):** Standardized Principal Component Analysis (`dudi.pca`) on the 9 active variables to map underlying preference axes.
3. **Dual Hierarchical Clustering (Ward's Method):**
   - *Activity Clustering:* Grouping variables to identify product ecosystems.
   - *User Segmentation:* Silhouette-validated clustering `k = 2` to isolate high-value consumer segments.

<br>

## 3. Key Findings & Strategic Recommendations

- **Three Distinct Activity Ecosystems:** Hierarchical clustering on transposed variables reveals that leisure activities are not consumed independently, but in three coherent bundles:
  1. *Free & Spiritual* (Religious institutions, Beaches)
  2. *Accommodation & Nightlife* (Resorts, Dance clubs, Juice bars)
  3. *Culture & Gastronomy* (Museums, Art galleries, Theaters, Restaurants)
- **Asymmetric User Segmentation (80/20 Split):** User clustering isolates a dominant segment (~80% of the user base) driving engagement across paid infrastructure and integrated complexes, distinct from low-spend/free-activity travelers.
- **Actionable Strategy:**
  - **Platform Algorithm:** Transition from location-based suggestions to semantic bundle recommendations within the same cluster.
  - **Capital Allocation:** Focus B2B partnerships and investor capital (~80% weight) on integrated hospitality complexes combining accommodation, dining, and cultural venues.
