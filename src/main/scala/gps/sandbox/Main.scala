/* This file is part of GauProS.
 * https://github.com/DirkToewe/gaupros_sandbox
 *
 * GauProS is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GauProS is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GauProS.  If not, see <https://www.gnu.org/licenses/>.
 */

package gps.sandbox

import java.nio.ByteBuffer
import java.util.Base64
import org.scalajs.dom.raw.SVGLength.SVG_LENGTHTYPE_PERCENTAGE

import gps.kernel._
import gps.linalg.Vec
import gps.regression.GPR
import gps.regression.gpr.LikelihoodFunction

import math.{pow, sqrt => √}
import org.scalajs.dom.{document => doc}
import org.scalajs.dom.raw._

/**
  * Created by Dirk Toewe on 18.06.17.
  */
object Main
{
  val SVG_NS  = "http://www.w3.org/2000/svg"

  def main( args: Array[String] ): Unit =
  {
    val input_sigma     = doc.getElementById(     "sigma_spinner").asInstanceOf[HTMLInputElement]
    val input_shift     = doc.getElementById(     "shift_spinner").asInstanceOf[HTMLInputElement]
    val input_noise     = doc.getElementById(     "noise_spinner").asInstanceOf[HTMLInputElement]
    val input_kernel_exp= doc.getElementById("kernel_exp_spinner").asInstanceOf[HTMLInputElement]
    val input_σ_f       = doc.getElementById(       "σ_f_spinner").asInstanceOf[HTMLInputElement]

    val input_min_sigma = doc.getElementById("sigma_spinner_min" ).asInstanceOf[HTMLInputElement]
    val input_min_shift = doc.getElementById("shift_spinner_min" ).asInstanceOf[HTMLInputElement]
    val input_min_noise = doc.getElementById("noise_spinner_min" ).asInstanceOf[HTMLInputElement]
    val input_min_σ_f   = doc.getElementById(  "σ_f_spinner_min" ).asInstanceOf[HTMLInputElement]

    val input_max_sigma = doc.getElementById("sigma_spinner_max" ).asInstanceOf[HTMLInputElement]
    val input_max_shift = doc.getElementById("shift_spinner_max" ).asInstanceOf[HTMLInputElement]
    val input_max_noise = doc.getElementById("noise_spinner_max" ).asInstanceOf[HTMLInputElement]
    val input_max_σ_f   = doc.getElementById(  "σ_f_spinner_max" ).asInstanceOf[HTMLInputElement]

    val btn_fit_marginal= doc.getElementById("btn_fit_marginal"  ).asInstanceOf[HTMLButtonElement]
    val btn_fit_loo     = doc.getElementById("btn_fit_loo"       ).asInstanceOf[HTMLButtonElement]
    val btn_fit_educated= doc.getElementById("btn_fit_educated"  ).asInstanceOf[HTMLButtonElement]

    val svg_root        = doc.getElementById("svg_root"          ).asInstanceOf[SVGSVGElement]
    val svg_points      = doc.getElementById("svg_points"        ).asInstanceOf[SVGSVGElement]
    val svg_curve_cov   = doc.getElementById("svg_curve_cov"     ).asInstanceOf[SVGPolylineElement]
    val svg_curve_mean  = doc.getElementById("svg_curve_mean"    ).asInstanceOf[SVGPolylineElement]

    val console_div = doc.getElementById("console_div").asInstanceOf[HTMLDivElement]

    def update(): Unit =
    {
      val pts = svg_points.getElementsByTagName("circle")
      val x =Array.tabulate(pts.length){pts(_).asInstanceOf[SVGCircleElement].cx.baseVal.value}
      val y =  Vec.tabulate(pts.length){pts(_).asInstanceOf[SVGCircleElement].cy.baseVal.value}

      try {
        val buf = ByteBuffer allocate (x.length*8 + 20)

        buf putFloat input_σ_f       .valueAsNumber.toFloat
        buf putFloat input_noise     .valueAsNumber.toFloat
        buf putFloat input_shift     .valueAsNumber.toFloat
        buf putFloat input_kernel_exp.valueAsNumber.toFloat
        buf putFloat input_sigma     .valueAsNumber.toFloat

        var    i = 0
        while( i < x.length ) {
          buf putFloat x(i).toFloat
          buf putFloat y(i).toFloat
          i += 1
        }

        doc.location.hash = Base64.getEncoder encodeToString buf.array
      }
      catch {
        case err: Throwable =>
          console_div.innerHTML = err.toString
          err.printStackTrace()
          throw err
      }

      val σ_f  =            input_σ_f  .     valueAsNumber
      val σ_n  =            input_noise.     valueAsNumber
      val shift=            input_shift.     valueAsNumber
      val p    =            input_kernel_exp.valueAsNumber
      val θ    = 0.5 / pow( input_sigma.     valueAsNumber, 2 )

      val kernel = Noise(σ_n pow 2) + σ_f.pow(2) * Exp( -θ * AbsDelta.pow(p) ) // <- TODO one could also use θ = log(t) here and move t out of the exponent

      val gpr = try {
        GPR(x, y, shift, kernel)
      }
      catch {
        case err: Throwable =>
          console_div.innerHTML = err.toString
          err.printStackTrace()
          throw err
      }
      console_div.innerHTML = ""

      val xStart =          Math.floor(svg_root.getBoundingClientRect().left ).toInt
      val xEnd   = xStart + Math.ceil (svg_root.getBoundingClientRect().width).toInt
      val pixel = svg_root.createSVGPoint
      val samples = xStart to xEnd by 1 map {
        x =>
          pixel.x = x
          val pt = pixel matrixTransform svg_root.getScreenCTM.inverse
          val (mean,cov) = gpr.mean_var(pt.x)
          (pt.x, mean, √{cov})
      }

      var points_mean= StringBuilder.newBuilder
      var points_var = StringBuilder.newBuilder
      for( (x,mean, _ ) <- samples        ) points_mean++= f"$x%.2f $mean%.2f,"
      for( (x,mean,std) <- samples        ) points_var ++= f"$x%.2f ${mean + std}%.2f,"
      for( (x,mean,std) <- samples.reverse) points_var ++= f"$x%.2f ${mean - std}%.2f,"
      points_mean.deleteCharAt(points_mean.size-1)
      points_var .deleteCharAt(points_var .size-1)
      // highlight +-2σ band
      svg_curve_mean.setAttributeNS(null, "points", points_mean.toString)
      svg_curve_cov .setAttributeNS(null, "points", points_var .toString)
    }

    def fit( likelihood: (Array[Double],Vec,Symbol,Kernel[Double]) => LikelihoodFunction ) =
    {
      val pts = svg_points.getElementsByTagName("circle")
      val x =Array.tabulate(pts.length){pts(_).asInstanceOf[SVGCircleElement].cx.baseVal.value}
      val y =  Vec.tabulate(pts.length){pts(_).asInstanceOf[SVGCircleElement].cy.baseVal.value}

      val norm_p = input_kernel_exp.valueAsNumber

      val kernel = Noise('σ_n pow 2) + ('σ_f pow 2) * Exp( -'θ * AbsDelta.pow(norm_p) )
      val param_min = Map(
        'σ_n     ->            input_min_noise.valueAsNumber,
        'σ_f     ->            input_min_σ_f  .valueAsNumber,
        'θ       -> 0.5 / pow( input_max_sigma.valueAsNumber, 2 ),
        'y_shift ->            input_min_shift.valueAsNumber
      )
      val param_max = Map(
        'σ_n     ->            input_max_noise.valueAsNumber,
        'σ_f     ->            input_max_σ_f  .valueAsNumber,
        'θ       -> 0.5 / pow( input_min_sigma.valueAsNumber, 2 ),
        'y_shift ->            input_max_shift.valueAsNumber
      )
      var param_init = Map(
        'σ_n     ->            input_noise.valueAsNumber,
        'σ_f     ->            input_σ_f  .valueAsNumber,
        'θ       -> 0.5 / pow( input_sigma.valueAsNumber, 2 ),
        'y_shift ->            input_shift.valueAsNumber
      )
      for( (k,v) <- param_init )
        param_init = param_init updated ( k, v max param_min(k)
                                               min param_max(k) )

      val gpr = try {
        console_div.innerHTML = "fitting..."
        val gpr = GPR.fit_shifted[Double](x,y, 'y_shift, kernel, likelihood, param_init, param_min, param_max)

        input_noise.valueAsNumber =          gpr.params('σ_n)
        input_σ_f  .valueAsNumber =          gpr.params('σ_f)
        input_sigma.valueAsNumber = √( 0.5 / gpr.params('θ) )
        input_shift.valueAsNumber =          gpr.params('y_shift)

        input_noise.onchange(null)
        input_σ_f  .onchange(null)
        input_sigma.onchange(null)
        input_shift.onchange(null)
      }
      catch {
        case err: Throwable =>
          console_div.innerHTML = err.toString
          err.printStackTrace()
          throw err
      }
      console_div.innerHTML = ""
    }

    def fit_educated() =
    {
      val pts = svg_points.getElementsByTagName("circle")
      val x =Array.tabulate(pts.length){pts(_).asInstanceOf[SVGCircleElement].cx.baseVal.value}
      val y =  Vec.tabulate(pts.length){pts(_).asInstanceOf[SVGCircleElement].cy.baseVal.value}

      val y_mean = y.sum / y.length; val y_std  = √( y.map{ y => (y-y_mean)*(y-y_mean) }.sum / y.length )
      val x_mean = x.sum / x.length; val x_std  = √( x.map{ x => (x-x_mean)*(x-x_mean) }.sum / x.length )

      input_σ_f       .valueAsNumber = y_std
      input_noise     .valueAsNumber = y_std / 8
      input_shift     .valueAsNumber = y_mean
      input_kernel_exp.valueAsNumber = 1.9
      input_sigma     .valueAsNumber = x_std * 8 / x.length

      update()
    }

    btn_fit_marginal.onclick = _ => fit( GPR.logp_marginal[Double] )
    btn_fit_loo     .onclick = _ => fit( GPR.logp_loo     [Double] )
    btn_fit_educated.onclick = _ => fit_educated()

    // synchronize sliders and their corresponding spinners
    for( (spinner,sliderStr) <- Seq(
      (input_sigma,          "sigma_slider"),
      (input_shift,          "shift_slider"),
      (input_noise,          "noise_slider"),
      (input_kernel_exp,"kernel_exp_slider"),
      (input_σ_f,              "σ_f_slider")
    ) )
    {
      val slider = doc.getElementById(sliderStr).asInstanceOf[HTMLInputElement]
      slider.onchange = _ => {
        spinner.value = slider.value
        update()
      }
      spinner.onchange = _ => {
        slider.value = spinner.value
        update()
      }
    }

    // INSTALL MOUSE CONTROLS
    val pt = svg_root.createSVGPoint
    svg_root.onmousedown = evt =>
      if( ! evt.shiftKey ) {
        if( evt.ctrlKey ) {
          svg_points.removeChild( evt.target.asInstanceOf[SVGCircleElement] )
          update()
        }
      }
      else {
        pt.x = evt.clientX
        pt.y = evt.clientY
        val center = pt matrixTransform svg_root.getScreenCTM.inverse
        val circle = doc.createElementNS(SVG_NS, "circle").asInstanceOf[SVGCircleElement]
        circle.cx.baseVal.value = center.x
        circle.cy.baseVal.value = center.y
        circle.r .baseVal.newValueSpecifiedUnits(SVG_LENGTHTYPE_PERCENTAGE, 0.4)
        svg_points.appendChild(circle)
        update()
      }
    var point2move = null: SVGCircleElement
    svg_points.onmousedown
      = evt => if( evt.button == 0 && ! evt.shiftKey )
          point2move = evt.target.asInstanceOf[SVGCircleElement]
    svg_root.onmousemove
      = evt => if( null != point2move ) {
          pt.x = evt.clientX
          pt.y = evt.clientY
          val center = pt.matrixTransform(svg_root.getScreenCTM.inverse)
          point2move.cx.baseVal.value = center.x
          point2move.cy.baseVal.value = center.y
          update()
        }
    val on_drag_end = (_: MouseEvent) => point2move = null
    svg_root.onmouseup = on_drag_end

    // LOAD HASH DATA (IF PRESENT)
    if( doc.location.hash.startsWith("#") )
      try
      {
        val buf = ByteBuffer.wrap{
          Base64.getDecoder decode doc.location.hash.drop(1)
        }.asFloatBuffer

        if( buf.remaining() >= 5 )
        {
          input_σ_f       .valueAsNumber = buf.get()
          input_noise     .valueAsNumber = buf.get()
          input_shift     .valueAsNumber = buf.get()
          input_kernel_exp.valueAsNumber = buf.get()
          input_sigma     .valueAsNumber = buf.get()
        }

        while( buf.remaining > 1 )
        {
          val pt = svg_root.createSVGPoint
          val circle = doc.createElementNS(SVG_NS, "circle").asInstanceOf[SVGCircleElement]
          circle.cx.baseVal.value = buf.get()
          circle.cy.baseVal.value = buf.get()
          circle.r .baseVal.newValueSpecifiedUnits(SVG_LENGTHTYPE_PERCENTAGE, 0.4)
          svg_points.appendChild(circle)
        }

        update()
      }
      catch {
        case err: Throwable =>
          console_div.innerHTML = err.toString
          err.printStackTrace()
          throw err
      }
  }
}
