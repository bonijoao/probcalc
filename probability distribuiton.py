import streamlit as st
import numpy as np
from scipy import stats
import plotly.graph_objects as go
import plotly.express as px
from typing import Dict, Any
import math

# Configurações da página
st.set_page_config(
    page_title="ProbCalc",
    layout="wide"
)

# Layout principal
st.title("ProbCalc")

# Colunas para parâmetros e visualização
col1, col2 = st.columns([1, 2])

with col1:
    st.subheader("Parâmetros")
    
    # Seleção da distribuição
    dist = st.selectbox(
        "Distribuição:",
        options=[
            "norm", "t", "chisq", "f", "gamma", "lnorm",
            "pareto", "weibull", "binom", "geom1", "geom2",
            "hyper", "nbin1", "nbin2", "beta", "exp", "pois"
        ],
        format_func=lambda x: {
            "norm": "Normal",
            "t": "t-Student",
            "chisq": "Qui-quadrado",
            "f": "F",
            "gamma": "Gamma",
            "lnorm": "Log-normal",
            "pareto": "Pareto",
            "weibull": "Weibull",
            "binom": "Binomial",
            "geom1": "Geométrica (tentativas)",
            "geom2": "Geométrica (fracassos)",
            "hyper": "Hipergeométrica",
            "nbin1": "Binomial Negativa (tentativas)",
            "nbin2": "Binomial Negativa (fracassos)",
            "beta": "Beta",
            "exp": "Exponencial",
            "pois": "Poisson"
        }[x]
    )

    # Parâmetros específicos para cada distribuição
    params = {}
    
    if dist == "norm":
        params["mean"] = st.number_input("Média:", value=0.0)
        params["sd"] = st.number_input("Desvio Padrão:", value=1.0, min_value=0.1)
    elif dist == "t":
        params["df"] = st.number_input("Graus de Liberdade:", value=5, min_value=1)
    elif dist == "chisq":
        params["df"] = st.number_input("Graus de Liberdade:", value=1, min_value=1)
    elif dist == "f":
        params["df1"] = st.number_input("Graus de Liberdade 1:", value=1, min_value=1)
        params["df2"] = st.number_input("Graus de Liberdade 2:", value=1, min_value=1)
    elif dist == "gamma":
        params["shape"] = st.number_input("α (shape):", value=2.0, min_value=0.1)
        params["scale"] = st.number_input("β (scale):", value=1.0, min_value=0.1)
    elif dist == "lnorm":
        params["meanlog"] = st.number_input("μ (média log):", value=0.0)
        params["sdlog"] = st.number_input("σ (desvio padrão log):", value=1.0, min_value=0.1)
    elif dist == "pareto":
        params["m"] = st.number_input("m (mínimo):", value=1.0, min_value=0.1)
        params["alpha"] = st.number_input("α (shape):", value=3.0, min_value=0.1)
    elif dist == "weibull":
        params["alpha"] = st.number_input("α (shape):", value=2.0, min_value=0.1)
        params["beta"] = st.number_input("β (scale):", value=1.0, min_value=0.1)
    elif dist == "binom":
        params["n"] = st.number_input("n (tentativas):", value=10, min_value=1, step=1)
        params["p"] = st.number_input("p (probabilidade):", value=0.5, min_value=0.0, max_value=1.0)
    elif dist in ["geom1", "geom2"]:
        params["p"] = st.number_input("p (probabilidade de sucesso):", value=0.5, min_value=0.001, max_value=1.0)
    elif dist == "hyper":
        params["N"] = st.number_input("N (tamanho da população):", value=100, min_value=1, step=1)
        params["K"] = st.number_input("K (número de sucessos na população):", value=50, min_value=1, step=1)
        params["n"] = st.number_input("n (tamanho da amostra):", value=10, min_value=1, step=1)
    elif dist in ["nbin1", "nbin2"]:
        params["r"] = st.number_input("r (sucessos desejados):", value=5, min_value=1, step=1)
        params["p"] = st.number_input("p (probabilidade de sucesso):", value=0.5, min_value=0.001, max_value=1.0)
    elif dist == "beta":
        params["alpha"] = st.number_input("α (shape1):", value=2.0, min_value=0.1)
        params["beta"] = st.number_input("β (shape2):", value=2.0, min_value=0.1)
    elif dist == "exp":
        params["lambda"] = st.number_input("λ (taxa):", value=1.0, min_value=0.1)
    elif dist == "pois":
        params["lambda"] = st.number_input("λ (taxa):", value=5.0, min_value=0.1)

    # Tipo de probabilidade
    if dist in ["binom", "pois", "geom1", "geom2", "hyper", "nbin1", "nbin2"]:
        prob_type = st.selectbox(
            "Tipo de Probabilidade:",
            options=["greater_eq", "less_eq", "equal"],
            format_func=lambda x: {
                "greater_eq": "P(X ≥ x)",
                "less_eq": "P(X ≤ x)",
                "equal": "P(X = x)"
            }[x]
        )
    else:
        prob_type = st.selectbox(
            "Tipo de Probabilidade:",
            options=["greater", "less", "both"],
            format_func=lambda x: {
                "greater": "P(X > x)",
                "less": "P(X < x)",
                "both": "2P(|X| > x)"
            }[x]
        )

    # Entrada por probabilidade
    prob_input = st.checkbox("Entrada por probabilidade")

    if not prob_input:
        x_value = st.number_input("Valor de x:", value=0.0)
    else:
        prob_value = st.number_input("Probabilidade:", value=0.95, min_value=0.0, max_value=1.0)

    calculate = st.button("Calcular")

# Funções auxiliares para distribuição de Pareto
def dpareto(x: np.ndarray, m: float, alpha: float) -> np.ndarray:
    return np.where(x >= m, alpha * m**alpha / x**(alpha + 1), 0)

def ppareto(q: float, m: float, alpha: float) -> float:
    return np.where(q >= m, 1 - (m/q)**alpha, 0)

def qpareto(p: float, m: float, alpha: float) -> float:
    return m / (1 - p)**(1/alpha)

# Função para calcular probabilidades
def calculate_probability(dist: str, params: Dict[str, float], x: float, prob_type: str) -> float:
    if dist == "norm":
        if prob_type == "greater":
            return 1 - stats.norm.cdf(x, params["mean"], params["sd"])
        elif prob_type == "less":
            return stats.norm.cdf(x, params["mean"], params["sd"])
        else:  # both
            return 2 * (1 - stats.norm.cdf(abs(x), params["mean"], params["sd"]))
    
    elif dist == "t":
        if prob_type == "greater":
            return 1 - stats.t.cdf(x, params["df"])
        elif prob_type == "less":
            return stats.t.cdf(x, params["df"])
        else:  # both
            return 2 * (1 - stats.t.cdf(abs(x), params["df"]))
    
    elif dist == "binom":
        if prob_type == "greater_eq":
            return 1 - stats.binom.cdf(x-1, params["n"], params["p"])
        elif prob_type == "less_eq":
            return stats.binom.cdf(x, params["n"], params["p"])
        else:  # equal
            return stats.binom.pmf(x, params["n"], params["p"])
    
    # Adicione as outras distribuições aqui...

# Função para calcular x dado uma probabilidade
def calculate_x_from_prob(dist: str, params: Dict[str, float], prob: float, prob_type: str) -> float:
    if dist == "norm":
        if prob_type == "greater":
            return stats.norm.ppf(1 - prob, params["mean"], params["sd"])
        elif prob_type == "less":
            return stats.norm.ppf(prob, params["mean"], params["sd"])
        else:  # both
            return stats.norm.ppf(1 - prob/2, params["mean"], params["sd"])
    
    elif dist == "t":
        if prob_type == "greater":
            return stats.t.ppf(1 - prob, params["df"])
        elif prob_type == "less":
            return stats.t.ppf(prob, params["df"])
        else:  # both
            return stats.t.ppf(1 - prob/2, params["df"])
    
    elif dist == "binom":
        if prob_type == "greater_eq":
            return stats.binom.ppf(1 - prob, params["n"], params["p"])
        elif prob_type == "less_eq":
            return stats.binom.ppf(prob, params["n"], params["p"])
        else:  # equal
            return None
    
    # Adicione as outras distribuições aqui...

# Função para gerar dados para o gráfico
def generate_plot_data(dist: str, params: Dict[str, float]) -> tuple:
    if dist in ["binom", "pois", "geom1", "geom2", "hyper", "nbin1", "nbin2"]:  # Distribuições discretas
        if dist == "binom":
            x = np.arange(0, params["n"] + 1)
            y = stats.binom.pmf(x, params["n"], params["p"])
        elif dist == "pois":
            x = np.arange(0, max(20, stats.poisson.ppf(0.999, params["lambda"])))
            y = stats.poisson.pmf(x, params["lambda"])
        # Adicione as outras distribuições discretas aqui...
        
        return x, y, True  # True indica distribuição discreta
    
    else:  # Distribuições contínuas
        if dist == "norm":
            x = np.linspace(params["mean"] - 4*params["sd"], 
                          params["mean"] + 4*params["sd"], 200)
            y = stats.norm.pdf(x, params["mean"], params["sd"])
        elif dist == "t":
            x = np.linspace(stats.t.ppf(0.001, params["df"]),
                          stats.t.ppf(0.999, params["df"]), 200)
            y = stats.t.pdf(x, params["df"])
        # Adicione as outras distribuições contínuas aqui...
        
        return x, y, False  # False indica distribuição contínua

# Visualização
with col2:
    st.subheader("Visualização")
    
    if calculate:
        # Gerar dados para o gráfico
        x_plot, y_plot, is_discrete = generate_plot_data(dist, params)
        
        # Criar gráfico base
        fig = go.Figure()
        
        # Adicionar traço principal
        if is_discrete:
            fig.add_trace(go.Bar(
                x=x_plot,
                y=y_plot,
                name="Probabilidade",
                marker_color="lightblue"
            ))
        else:
            fig.add_trace(go.Scatter(
                x=x_plot,
                y=y_plot,
                name="Densidade",
                mode="lines",
                line=dict(color="blue")
            ))
        
        # Calcular e mostrar resultado
        if not prob_input:
            prob = calculate_probability(dist, params, x_value, prob_type)
            result_text = f"P(X = {x_value:.4f}) = {prob:.4f}"
        else:
            x_result = calculate_x_from_prob(dist, params, prob_value, prob_type)
            result_text = f"X para P = {prob_value:.4f}: {x_result:.4f}"
        
        # Adicionar linha vertical para o valor de x
        if not prob_input:
            fig.add_vline(x=x_value, line_dash="dash", line_color="red")
        else:
            fig.add_vline(x=x_result, line_dash="dash", line_color="red")
        
        # Configurar layout
        fig.update_layout(
            title="Distribuição de Probabilidade",
            xaxis_title="X",
            yaxis_title="Densidade/Probabilidade",
            showlegend=False,
            template="simple_white"
        )
        
        # Mostrar gráfico e resultado
        st.plotly_chart(fig, use_container_width=True)
        st.markdown(f"### {result_text}")

