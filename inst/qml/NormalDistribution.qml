import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

import "./common" as Common

Form
{
	info: qsTr("Demonstration of the Normal (Gaussian) distribution")

	Common.Parametrization
	{
		id: parametrization
		values : [
			{ label: qsTr("std. dev"),  value: "sigma"  },
			{ label: qsTr("variance"),  value: "sigma2" },
			{ label: qsTr("precision"), value: "tau"    }
		]
	}

	Group
	{
		DoubleField{ name: "mu";		label: qsTr("Mean μ"); 		negativeValues: true; info: qsTr("Mean parameter") }
		DoubleField{ name: "sigma";		label: qsTr("Std. dev σ"); 	negativeValues: false; visible: parametrization.value === "sigma";	defaultValue: 1; info: qsTr("Std.dev")}
		DoubleField{ name: "sigma2";	label: qsTr("Variance σ²");	negativeValues: false; visible: parametrization.value === "sigma2";	defaultValue: 1; info: qsTr("Variance")}
		DoubleField{ name: "tau";		label: qsTr("Precision τ");	negativeValues: false; visible: parametrization.value === "tau";	defaultValue: 1; info: qsTr("Precision parameter")}
	}

	Common.ShowDistributionContinuous {}

	Common.GenerateDisplayData {}

	Section
	{
		title: qsTr("Estimate Parameters")

		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight / 2
			AvailableVariablesList 
			{ 
				name: "estimableParameters"
				label: qsTr("Estimable parameters")
				source: [{ values: [
					{label: qsTr("Mean μ"),		value: "mu"},
					{label: qsTr("Std. dev σ"),	value: "sigma"}
				] 
				}] 
			}
			AssignedVariablesList
			{ 
				name: "estimateParameters"
				label: qsTr("Parameters to estimate") 
			}
		}

		CheckBox 
		{
			name: "parameterTable"
			label: qsTr("Parameter table")

			CheckBox
			{
				name: "parameterTableUncertainty"; label: qsTr("Quantify uncertainty")
				CIField{ name: "parameterTableCiLevel"; label: qsTr("Confidence level") }
			}
		}

		CheckBox { name: "besselsCorrection"; label: qsTr("Bessels correction")}
	}

	Common.AssessFit{}

	Section
	{
		title: qsTr("Advanced options")

		RadioButton
		{
			name: "parameterEstimationMethod"
			title: qsTr("Parameter estimation method")
			id: parameterEstimationMethod
			info: qsTr("If available, parameter estimates are calculated analytically. If not, then parameters are estimated using numerical optimization of the log-likelihood function.")
			RadioButton
			{
				label:	qsTr("Unconstrained")
				value:	"unconstrained"
				info:	qsTr("Optimize the log-likelihood on the unconstrained parameter space. For example, a parameter that is always positive only is optimized on the log scale. The optimization method used is 'L-BFGS-B'.")
			}
			RadioButton
			{
				label:	qsTr("Constrained")
				value:	"constrained"
				info:	qsTr("Optimize the likelihood on the constrained parameter space. The optimization method used is 'BFGS'.")
			}
		}

		RadioButtonGroup
		{
			name: "parameterUncertaintyMethod"
			title: qsTr("Parameter uncertainty quantification method")
			info: qsTr("How to quantify uncertainty about the parameters")
			RadioButton 
			{ 
				label: qsTr("Default")
				value: "default"
				info: qsTr("If available, analytic SE and CIs are calculated, otherwise, they are calculated using normal theory based on numerical estimate of the Hessian.")

				CheckBox 
				{ 
					name:	"parameterUncertaintySymmetricCi"
					label:	qsTr("Symmetric CI")
					enabled: parameterEstimationMethod.value == "unconstrained"
					info:	qsTr("Should the CI be computed on the unconstrained space and then CI bounds transformed into the original parameter space, or should the CIs be computed on the constrained space using transformed Hessian. The latter produces CIs that are symmetric aroud the point estimate but may, in some extreme cases, produce CIs extending beyond support of the parameter.")}
				}
			}
			RadioButton 
			{ 
				label: qsTr("Profile likelihood")
				value: "profile"
				info: qsTr("CIs are calculated via profile likelihood.")
			}
			RadioButton 
			{ 
				label: qsTr("Bootstrap")
				value: "bootstrap"
				info: qsTr("SE and CIs are calculated using parametric bootstrap.")
				childrenOnSameRow: true
				IntegerField { name: "parameterUncertaintyBootstrapSamples"; label: qsTr("with"); afterLabel: qsTr("bootstrap samples"); defaultValue: 1000 }
			}
		}
}