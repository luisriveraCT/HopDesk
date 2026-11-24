"""T80: tests of the AWS trigger, without AWS or GitHub (fakes in deps).

Run from the repo root:  python -m unittest infra/avisos_disparo/test_lambda_function.py
"""
import os
import sys
import unittest
from datetime import datetime, timezone

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import lambda_function as lf  # noqa: E402

TOKEN = "ghp_falso_nunca_impreso"


class Falsos:
    def __init__(self, corridas):
        self.corridas_v, self.disparos, self.alertas, self.pedido = corridas, [], [], None

    def deps(self):
        return {
            "leer_token": lambda nombre: TOKEN,
            "corridas": self._corridas,
            "disparar": lambda tok, repo, wf, ref, modo: self.disparos.append((tok, repo, wf, ref, modo)),
            "alertar": lambda arn, modo, ahora: self.alertas.append((arn, modo)),
        }

    def _corridas(self, tok, repo, wf, desde):
        self.pedido = desde
        return self.corridas_v


def correr(evento, corridas, ahora=datetime(2026, 11, 23, 14, 0, tzinfo=timezone.utc)):
    f = Falsos(corridas)
    r = lf.lambda_handler(evento, None, ahora=ahora, deps=f.deps())
    return r, f


class T80Disparo(unittest.TestCase):
    def setUp(self):
        os.environ.update(REPO="dueno/repo", WORKFLOW="avisos.yml", REF="master",
                          TOKEN_PARAM="/hopdesk/avisos/github_token", SNS_TOPIC_ARN="arn:sns:falso")

    def test_sin_corridas_dispara_con_prueba_false_en_master(self):
        r, f = correr({"modo": "lunes"}, [])
        self.assertEqual(r["decision"], "disparar")
        self.assertEqual(f.disparos, [(TOKEN, "dueno/repo", "avisos.yml", "master", "lunes")])
        self.assertEqual(f.alertas, [])

    def test_una_real_exitosa_de_la_semana_no_dispara(self):
        r, f = correr({"modo": "lunes"}, [{"name": "avisos lunes real", "status": "completed", "conclusion": "success"}])
        self.assertEqual(r["decision"], "listo")
        self.assertEqual(f.disparos, [])

    def test_prueba_otro_modo_o_fallida_no_cuentan(self):
        corridas = [{"name": "avisos lunes prueba", "status": "completed", "conclusion": "success"},
                    {"name": "avisos viernes real", "status": "completed", "conclusion": "success"},
                    {"name": "avisos lunes real", "status": "completed", "conclusion": "failure"},
                    {"name": "Avisos semanales (D)", "status": "completed", "conclusion": "success"}]
        r, f = correr({"modo": "lunes"}, corridas)
        self.assertEqual(r["decision"], "disparar")
        self.assertEqual(len(f.disparos), 1)

    def test_una_en_curso_no_dispara_otra(self):
        r, f = correr({"modo": "viernes"}, [{"name": "avisos viernes real", "status": "in_progress", "conclusion": None}])
        self.assertEqual(r["decision"], "en_curso")
        self.assertEqual(f.disparos, [])

    def test_alerta_si_no_salio_y_ultimo_intento(self):
        r, f = correr({"modo": "lunes", "alertar": True}, [])
        self.assertTrue(r["alerta"])
        self.assertEqual(f.alertas, [("arn:sns:falso", "lunes")])
        self.assertEqual(len(f.disparos), 1)

    def test_alerta_no_suena_si_ya_salio(self):
        r, f = correr({"modo": "lunes", "alertar": True},
                      [{"name": "avisos lunes real", "status": "completed", "conclusion": "success"}])
        self.assertFalse(r["alerta"])
        self.assertEqual(f.alertas, [])

    def test_alerta_suena_con_una_en_curso_pero_no_dispara(self):
        r, f = correr({"modo": "viernes", "alertar": True},
                      [{"name": "avisos viernes real", "status": "queued", "conclusion": None}])
        self.assertTrue(r["alerta"])
        self.assertEqual(f.disparos, [])

    def test_modo_invalido_falla(self):
        with self.assertRaises(ValueError):
            correr({"modo": "martes"}, [])

    def test_la_semana_empieza_el_lunes_0h_de_mexico(self):
        # Monday 2026-11-23 03:00 UTC is still Sunday 21:00 in Mexico: week of the 16th.
        self.assertEqual(lf.inicio_semana_utc(datetime(2026, 11, 23, 3, 0, tzinfo=timezone.utc)),
                         datetime(2026, 11, 16, 6, 0, tzinfo=timezone.utc))
        # Friday 23:30 Mexico (Saturday 05:30 UTC): week of the 16th.
        self.assertEqual(lf.inicio_semana_utc(datetime(2026, 11, 21, 5, 30, tzinfo=timezone.utc)),
                         datetime(2026, 11, 16, 6, 0, tzinfo=timezone.utc))
        _, f = correr({"modo": "lunes"}, [], ahora=datetime(2026, 11, 23, 14, 0, tzinfo=timezone.utc))
        self.assertEqual(f.pedido, datetime(2026, 11, 23, 6, 0, tzinfo=timezone.utc))

    def test_el_resultado_y_el_log_nunca_llevan_el_token(self):
        import io
        import contextlib
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            r, _ = correr({"modo": "archivo"}, [])
        self.assertNotIn(TOKEN, buf.getvalue())
        self.assertNotIn(TOKEN, repr(r))

    def test_un_error_http_no_lleva_el_cuerpo_de_github(self):
        import urllib.error
        def falla(req, timeout):
            raise urllib.error.HTTPError(req.full_url, 401, "Bad credentials", {}, None)
        original = lf.urllib.request.urlopen
        lf.urllib.request.urlopen = falla
        try:
            with self.assertRaises(RuntimeError) as cm:
                lf.disparar(TOKEN, "dueno/repo", "avisos.yml", "master", "lunes")
        finally:
            lf.urllib.request.urlopen = original
        self.assertIn("401", str(cm.exception))
        self.assertNotIn(TOKEN, str(cm.exception))


if __name__ == "__main__":
    unittest.main()
