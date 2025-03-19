import React, { FC } from 'react';
import { Modal } from 'react-bootstrap';
import i18next from 'i18next';

export const FAQModal: FC<{ show: boolean; onHide: () => void }> = ({ show, onHide }) => (
  <Modal show={show} onHide={onHide} fullscreen dialogClassName="slide-in-bottom">
    <Modal.Header closeButton>
      <Modal.Title>MyContexts FAQ's</Modal.Title>
    </Modal.Header>
    <Modal.Body>
      <ul>
        <details>
          <summary>{i18next.t("faqTitle_Wat_kan_ik_met_MyContexts_doen", {ns: 'mycontexts'})}</summary>
          <p>{i18next.t("faqContent_Wat_kan_ik_met_MyContexts_doen", {ns: 'mycontexts'})}</p>
        </details>
        <details>
          <summary>{i18next.t("faqTitle_Hoe_werkt_MyContexts", {ns: 'mycontexts'})}</summary>
          <p>{i18next.t("faqContent_Hoe_werkt_MyContexts", {ns: 'mycontexts'})}</p>
        </details>
        <details>
          <summary>{i18next.t("faqTitle_Waarom_is_MyContext_veilig", {ns: 'mycontexts'})}</summary>
          <p>{i18next.t("faqContent_Waarom_is_MyContext_veilig", {ns: 'mycontexts'})}</p>
        </details>
        <details>
          <summary>{i18next.t("faqTitle_Waarom_is_MyContexts_duurzaam", {ns: 'mycontexts'})}</summary>
          <p>{i18next.t("faqContent_Waarom_is_MyContexts_duurzaam", {ns: 'mycontexts'})}</p>
        </details>
        <details>
          <summary>{i18next.t("faqTitle_Wat_is_het_verschil_met_bijvoorbeeld_WhatsApp", {ns: 'mycontexts'})}</summary>
          <p>{i18next.t("faqContent_Wat_is_het_verschil_met_bijvoorbeeld_WhatsApp", {ns: 'mycontexts'})}</p>
        </details>
        <details>
          <summary>{i18next.t("faqTitle_Waarom_kost_MyConexts_zo_weinig", {ns: 'mycontexts'})}</summary>
          <p>{i18next.t("faqContent_Waarom_kost_MyConexts_zo_weinig", {ns: 'mycontexts'})}</p>
        </details>
        <details>
          <summary>{i18next.t("faqTitle_Stel_mijn_smartphone_gaat_kapot_Ben_ik_dan_alles_kwijt", {ns: 'mycontexts'})}</summary>
          <p>{i18next.t("faqContent_Stel_mijn_smartphone_gaat_kapot_Ben_ik_dan_alles_kwijt", {ns: 'mycontexts'})}</p>
        </details>
        <details>
          <summary>{i18next.t("faqTitle_Werkt_MyContexts_ook_op_mijn_andere_apparaten", {ns: 'mycontexts'})}</summary>
          <p>{i18next.t("faqContent_Werkt_MyContexts_ook_op_mijn_andere_apparaten", {ns: 'mycontexts'})}</p>
        </details>
        <details>
          <summary>{i18next.t("faqTitle_Kan_ik_ook_zelf_MyContexts_Apps_maken", {ns: 'mycontexts'})}</summary>
          <p>{i18next.t("faqContent_Kan_ik_ook_zelf_MyContexts_Apps_maken", {ns: 'mycontexts'})}</p>
        </details>
        <details>
          <summary>{i18next.t("faqTitle_Wie_heeft_MyContexts_gemaakt", {ns: 'mycontexts'})}</summary>
          <p>{i18next.t("faqContent_Wie_heeft_MyContexts_gemaakt", {ns: 'mycontexts'})}</p>
        </details>
        <details>
          <summary>{i18next.t("faqTitle_Waarom_is_dit_nooit_eerder_gedaan", {ns: 'mycontexts'})}</summary>
          <p>{i18next.t("faqContent_Waarom_is_dit_nooit_eerder_gedaan", {ns: 'mycontexts'})}</p>
        </details>
        <details>
          <summary>{i18next.t("faqTitle_Waar_kan_ik_meer_informatie_vinden_over_MyContexts", {ns: 'mycontexts'})}</summary>
          <p>{i18next.t("faqContent_Waar_kan_ik_meer_informatie_vinden_over_MyContexts", {ns: 'mycontexts'})}</p>
        </details>
      </ul>
    </Modal.Body>
  </Modal>
);

