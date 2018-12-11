---
# heading of document
title: # Chinese name（English name）
author:
date: # format 2006-01-01
# slug
slug: # code-English name
# specific parameters for current task
code:
sdk:
# hugo taxonomy related
tasknames:
categories:
# blogdown output
output:
  blogdown::html_page:
    toc: true
    md_extensions: -ascii_identifiers
---

```{r parse-meta, include=FALSE}
NAMES <- strsplit(rmarkdown::metadata$title, "（|）")[[1]]
NAME_CN <- NAMES[1]
NAME_EN <- NAMES[2]
CODE <- rmarkdown::metadata$code
SDK <- rmarkdown::metadata$sdk
```

# 基本信息

* **显示名称**：`r NAME_CN`
* **英文名称**：`r NAME_EN`
* **题目编号**：`r CODE`
* **任务描述**：
* **测查能力**：
* **相关任务**：

# 前端交互

## 基本逻辑

待完善。

## 详细说明

待完善。

# 后台配置

## 游戏参数

```{r configs, echo=FALSE, message=FALSE}
# knitr::kable(
#   readr::read_csv(
#     here::here("R", "config", NAME_EN, "configs.csv")
#   ),
#   caption = "配置参数总表"
# )
```

## 数据记录

```{r recording-variables, echo=FALSE, message=FALSE}
# knitr::kable(
#   readr::read_csv(
#     here::here("R", "config", NAME_EN, "recs.csv")
#   ),
#   caption = "原始数据变量列表"
# )
```

## 试题算分

```{r sdk-score, echo=FALSE, message=FALSE}
# knitr::kable(
#   readr::read_csv(
#     here::here("R", "config", NAME_EN, "sdk.csv")
#   ),
#   caption = paste0("传入参数（算分码：`", SDK, "`）")
# )
```
